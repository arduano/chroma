use std::borrow::Cow;

use crate::lang::{
    solver::{ModItemSet, TypeData},
    tokens::{Span, TkIdent},
};

use super::{
    TyType, TyTypeKind, TyTypeLogic, TypeAssignabilityQuery, TypeDependencies, TypeId,
    TypeRelationships, TypeSubsetQuery,
};

#[derive(Debug, Clone)]
pub struct TyUnion {
    /// List of references to union types.
    /// This list can't contain other unions.
    pub types: Vec<TypeId>,
}

fn is_type_subset_of(
    ty: TypeId,
    ty2: TypeId,
    types: &TypeData,
    type_relationships: &mut TypeRelationships,
) -> bool {
    if !type_relationships.is_type_subtype_of_type(types, ty, ty2) {
        return false;
    }
    true
}

fn is_type_subset_of_union(
    ty: TypeId,
    union_list: &Vec<TypeId>,
    types: &TypeData,
    type_relationships: &mut TypeRelationships,
) -> bool {
    if union_list.len() == 0 {
        return false;
    }

    for &union_ty in union_list {
        if !type_relationships.is_type_subtype_of_type(types, ty, union_ty) {
            return false;
        }
    }
    true
}

fn is_type_subset_of_union_with_query(
    ty: &TyType,
    union_list: &Vec<TypeId>,
    types: &TypeData,
    subset_query: &mut TypeSubsetQuery,
) -> bool {
    if union_list.len() == 0 {
        return false;
    }

    for union_ty in union_list {
        let union_ty = &types[union_ty];
        if !ty.is_substate_of(union_ty, subset_query) {
            return false;
        }
    }
    true
}

fn is_type_assignable_to_union(
    ty: &TyType,
    union_list: &Vec<TypeId>,
    assignability: &mut TypeAssignabilityQuery,
) -> bool {
    if union_list.len() == 0 {
        return false;
    }

    for union_ty in union_list {
        let union_ty = &assignability.types[union_ty];

        if ty.check_assignable_to(union_ty, assignability) {
            return true;
        }
    }
    false
}

impl TyUnion {
    pub fn new() -> Self {
        Self { types: Vec::new() }
    }

    pub fn new_with_types(types: Vec<TypeId>) -> Self {
        Self { types }
    }

    pub fn get_normalized_type_list(
        &self,
        types: &TypeData,
        type_relationships: &mut TypeRelationships,
    ) -> Option<Cow<'_, [TypeId]>> {
        let mut current = Cow::Borrowed(self.types.as_slice());

        // Normalize nested unions, appending nested union elements to the end until
        // there are no more nested unions.
        let mut i = 0;
        while i < current.len() {
            // Return None if the type doesn't exist. Recursive unions aren't allowed.
            let ty = types.get(current[i])?;

            if let TyTypeKind::Union(union) = &ty.kind {
                let current = current.to_mut();
                current.remove(i);
                for ty in &union.types {
                    current.push(ty.clone());
                }
            }

            i += 1;
        }

        // Normalize subset items. For each item, check if it's a subset of any
        // other item, and remove it if it is.
        let mut i = 0;
        while i < current.len() {
            let ty = current[i];
            for j in 0..current.len() {
                if i == j {
                    continue;
                }

                let ty2 = current[j];
                if is_type_subset_of(ty, ty2, types, type_relationships) {
                    current.to_mut().remove(i);
                    i -= 1;
                    break;
                }
            }

            i += 1;
        }

        Some(current)
    }

    pub fn insert_type_by_id_normalized(
        &mut self,
        ty_id: TypeId,
        types: &TypeData,
        type_relationships: &mut TypeRelationships,
    ) {
        let ty = types
            .get(ty_id)
            .expect("Incomplete expression when normalizing");

        if matches!(ty.kind, TyTypeKind::Never) {
            return;
        }

        if let TyTypeKind::Union(union) = &ty.kind {
            // Cloning here to keep the borrow checker happy
            let union_types_cloned = union.types.clone();
            for &ty in &union_types_cloned {
                self.insert_type_by_id_normalized(ty, types, type_relationships);
            }
            return;
        }

        // If it's already a substate, ignore
        if is_type_subset_of_union(ty_id, &self.types, types, type_relationships) {
            return;
        }

        // If it's a superset of any of the types, remove the types
        let mut i = 0;
        while i < self.types.len() {
            if is_type_subset_of(self.types[i], ty_id, &types, type_relationships) {
                self.types.remove(i);
            } else {
                i += 1;
            }
        }

        // Insert the type
        self.types.push(ty_id);
    }

    pub fn insert_type(&mut self, ty_id: TypeId, types_data: &mut TypeData) {
        self.types.push(ty_id);
    }

    pub fn union_types(
        left: TypeId,
        right: TypeId,
        span: Span,
        name: Option<TkIdent>,
        types_data: &mut TypeData,
    ) -> TyType {
        let mut new_union = TyUnion::new();

        new_union.insert_type(left, types_data);
        new_union.insert_type(right, types_data);

        let kind = TyTypeKind::Union(new_union);
        TyType::new_named(name, kind, span.clone())
    }

    pub fn union_types_as_id(
        left: TypeId,
        right: TypeId,
        span: Span,
        name: Option<TkIdent>,
        types_data: &mut TypeData,
    ) -> TypeId {
        let unioned = Self::union_types(left, right, span, name, types_data);
        types_data.add_value(unioned)
    }

    pub fn check_assignable_to_single(
        &self,
        other: &TyTypeKind,
        query: &mut TypeAssignabilityQuery,
    ) -> bool {
        // Every type in self must be assignable to other
        for ty in &self.types {
            let ty = &query.types[ty];
            if !ty.kind.check_assignable_to(other, query) {
                return false;
            }

            return false;
        }

        true
    }

    pub fn check_single_assignable_to_self(
        &self,
        other: &TyTypeKind,
        query: &mut TypeAssignabilityQuery,
    ) -> bool {
        // Other must be assignable to at least one type in self
        for ty in &self.types {
            let ty = &query.types[ty];
            if other.check_assignable_to(&ty.kind, query) {
                return true;
            }
        }

        false
    }

    pub fn is_substate_of_single(&self, other: &TyTypeKind, query: &mut TypeSubsetQuery) -> bool {
        // Every type in self must be a subset of other
        for ty in &self.types {
            let ty = &query.types[ty];
            if !ty.kind.is_substate_of(other, query) {
                return false;
            }
        }

        true
    }

    pub fn is_single_substate_of_self(
        &self,
        other: &TyTypeKind,
        query: &mut TypeSubsetQuery,
    ) -> bool {
        // Other must be a subset of at least one type in self
        for ty in &self.types {
            let ty = &query.types[ty];
            if other.is_substate_of(&ty.kind, query) {
                return true;
            }
        }

        false
    }
}

impl TyTypeLogic for TyUnion {
    fn check_assignable_to(&self, other: &Self, query: &mut TypeAssignabilityQuery) -> bool {
        // Every type in self must be assignable to at least one type in other
        for ty in &self.types {
            let ty = &query.types[ty];
            if is_type_assignable_to_union(&ty, &other.types, query) {
                continue;
            }

            return false;
        }

        true
    }

    fn is_substate_of(&self, other: &Self, query: &mut TypeSubsetQuery) -> bool {
        // Every type in self must be a subset of at least one type in other
        for ty in &self.types {
            let ty = &query.types[ty];
            if is_type_subset_of_union_with_query(&ty, &other.types, query.types, query) {
                continue;
            }

            return false;
        }

        true
    }

    fn get_intersection(&self, _other: &Self) -> Self {
        todo!()
    }

    fn get_type_dependencies(&self, types: &TypeData) -> TypeDependencies {
        let inner_types = self.types.iter().copied().collect();

        let mut normalization_deps = Vec::new();
        for &ty_id in &self.types {
            let Some(ty) = types.get(ty_id) else {
                // Can't normalize, bail
                return TypeDependencies {
                    inner_types,
                    ..Default::default()
                };
            };

            if matches!(ty.kind, TyTypeKind::Union(_)) {
                normalization_deps.push(ty_id);
            }
        }

        TypeDependencies {
            inner_types,
            ..Default::default()
        }
    }
}
