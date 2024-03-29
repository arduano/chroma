use std::borrow::Cow;

use crate::lang::{
    solver::{ModItemSet, TyIdOrValWithSpan, TypeData, TypeIdWithSpan},
    tokens::{Span, TkIdent},
};

use super::{
    TyType, TyTypeFlags, TyTypeKind, TyTypeLogic, TypeAssignabilityQuery, TypeDependencies,
    TypeSubsetQuery, TypeSubsetabilityCache,
};

#[derive(Debug, Clone)]
pub struct TyUnion {
    /// List of references to union types.
    /// This list can't contain other unions.
    pub types: Vec<TypeIdWithSpan>,
}

fn is_type_subset_of(
    ty: &TyType,
    ty2: &TyType,
    types: &ModItemSet<TyType>,
    type_subsetability: &mut TypeSubsetabilityCache,
) -> bool {
    if !ty.is_substate_of(ty2, &mut TypeSubsetQuery::new(types, type_subsetability)) {
        return false;
    }
    true
}

fn is_type_subset_of_union(
    ty: &TyType,
    union_list: &Vec<TypeIdWithSpan>,
    types: &ModItemSet<TyType>,
    type_subsetability: &mut TypeSubsetabilityCache,
) -> bool {
    if union_list.len() == 0 {
        return false;
    }

    for union_ty in union_list {
        let union_ty = &types[union_ty.id];
        if !ty.is_substate_of(
            union_ty,
            &mut TypeSubsetQuery::new(&types, type_subsetability),
        ) {
            return false;
        }
    }
    true
}

fn is_type_subset_of_union_with_query(
    ty: &TyType,
    union_list: &Vec<TypeIdWithSpan>,
    types: &ModItemSet<TyType>,
    subset_query: &mut TypeSubsetQuery,
) -> bool {
    if union_list.len() == 0 {
        return false;
    }

    for union_ty in union_list {
        let union_ty = &types[union_ty.id];
        if !ty.is_substate_of(union_ty, subset_query) {
            return false;
        }
    }
    true
}

fn is_type_assignable_to_union(
    ty: &TyType,
    union_list: &Vec<TypeIdWithSpan>,
    assignability: &mut TypeAssignabilityQuery,
) -> bool {
    if union_list.len() == 0 {
        return false;
    }

    for union_ty in union_list {
        let union_ty = &assignability.types[union_ty.id];

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

    pub fn new_with_types(types: Vec<TypeIdWithSpan>) -> Self {
        Self { types }
    }

    pub fn get_normalized_type_list(
        &self,
        types: &ModItemSet<TyType>,
        type_subsetability: &mut TypeSubsetabilityCache,
    ) -> Option<Cow<'_, [TypeIdWithSpan]>> {
        let mut current = Cow::Borrowed(self.types.as_slice());

        // Normalize nested unions, appending nested union elements to the end until
        // there are no more nested unions.
        let mut i = 0;
        while i < current.len() {
            // Return None if the type doesn't exist. Recursive unions aren't allowed.
            let ty = types.get(current[i].id)?;

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
            let ty = &types[current[i].id];
            for j in 0..current.len() {
                if i == j {
                    continue;
                }

                let ty2 = &types[current[j].id];
                if is_type_subset_of(ty, ty2, types, type_subsetability) {
                    current.to_mut().remove(i);
                    i -= 1;
                    break;
                }
            }

            i += 1;
        }

        Some(current)
    }

    /// An internal function to prepare a type for normalized inserting. Returns true if it should be
    /// inserted, false if not.
    fn prepare_type_for_normalized_inserting(
        &mut self,
        ty: &TyType,
        types: &ModItemSet<TyType>,
        type_subsetability: &mut TypeSubsetabilityCache,
    ) -> bool {
        if matches!(ty.kind, TyTypeKind::Never) {
            return false;
        }

        if let TyTypeKind::Union(union) = &ty.kind {
            // Cloning here to keep the borrow checker happy
            let union_types_cloned = union.types.clone();
            for ty in &union_types_cloned {
                self.insert_type_by_id_normalized(&ty, types, type_subsetability);
            }
            return false;
        }

        // If it's already a substate, ignore
        if is_type_subset_of_union(&ty, &self.types, types, type_subsetability) {
            return false;
        }

        // If it's a superset of any of the types, remove the types
        let mut i = 0;
        while i < self.types.len() {
            let union_ty = &types
                .get(self.types[i].id)
                .expect("Incomplete expression when normalizing");

            if is_type_subset_of(&union_ty, &ty, &types, type_subsetability) {
                self.types.remove(i);
            } else {
                i += 1;
            }
        }

        true
    }

    pub fn insert_type_by_id_normalized(
        &mut self,
        ty_ref: &TypeIdWithSpan,
        types: &ModItemSet<TyType>,
        type_subsetability: &mut TypeSubsetabilityCache,
    ) {
        let ty = types
            .get(ty_ref.id)
            .expect("Incomplete expression when normalizing");

        let should_insert =
            self.prepare_type_for_normalized_inserting(&ty, types, type_subsetability);

        if !should_insert {
            return;
        }

        // Insert the type
        self.types
            .push(TypeIdWithSpan::new(ty_ref.id, ty_ref.span.clone()));
    }

    pub fn insert_type_normalized(
        &mut self,
        ty_ref: TyIdOrValWithSpan,
        types: &mut ModItemSet<TyType>,
        type_subsetability: &mut TypeSubsetabilityCache,
    ) {
        let ty = types
            .get_val_for_val_or_id(&ty_ref.ty)
            .expect("Incomplete expression when normalizing");

        let should_insert =
            self.prepare_type_for_normalized_inserting(&ty, types, type_subsetability);

        if !should_insert {
            return;
        }

        let id = types.get_id_for_val_or_id(ty_ref.ty);

        // Insert the type
        self.types
            .push(TypeIdWithSpan::new(id, ty_ref.span.clone()));
    }

    pub fn insert_type(&mut self, ty_ref: TyIdOrValWithSpan, types_data: &mut TypeData) {
        let id = types_data.types.get_id_for_val_or_id(ty_ref.ty);

        // Insert the type
        self.types.push(TypeIdWithSpan::new(id, ty_ref.span));
    }

    pub fn union_types(
        left: TyIdOrValWithSpan,
        right: TyIdOrValWithSpan,
        span: Span,
        name: Option<TkIdent>,
        types_data: &mut TypeData,
    ) -> TyIdOrValWithSpan {
        let mut new_union = TyUnion::new();

        let left_flags = left.try_get_flags(&types_data.types);
        let right_flags = right.try_get_flags(&types_data.types);
        let flags = left_flags.join(right_flags);

        let mut insert_with_flags = |ty: TyIdOrValWithSpan, _flags: TyTypeFlags| {
            new_union.insert_type(ty, types_data);
        };

        insert_with_flags(left, flags);
        insert_with_flags(right, flags);

        let kind = TyTypeKind::Union(new_union);
        let ty = TyType::new_named(name, kind, span.clone(), flags);
        TyIdOrValWithSpan::new_val(ty, span)
    }

    pub fn check_assignable_to_single(
        &self,
        other: &TyTypeKind,
        query: &mut TypeAssignabilityQuery,
    ) -> bool {
        // Every type in self must be assignable to other
        for ty in &self.types {
            let ty = &query.types[ty.id];
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
            let ty = &query.types[ty.id];
            if other.check_assignable_to(&ty.kind, query) {
                return true;
            }
        }

        false
    }

    pub fn is_substate_of_single(&self, other: &TyTypeKind, query: &mut TypeSubsetQuery) -> bool {
        // Every type in self must be a subset of other
        for ty in &self.types {
            let ty = &query.types[ty.id];
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
            let ty = &query.types[ty.id];
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
            let ty = &query.types[ty.id];
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
            let ty = &query.types[ty.id];
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

    fn flags(&self, types: &ModItemSet<TyType>) -> TyTypeFlags {
        let mut flags = TyTypeFlags::new_all();

        for variant in &self.types {
            let ty = types.get(variant.id);
            if let Some(ty) = ty {
                flags = flags.join(ty.flags(types));
            } else {
                flags = flags.join(TyTypeFlags::new_for_unknown());
            }
        }

        flags
    }

    fn get_type_dependencies(&self, types: &ModItemSet<TyType>) -> TypeDependencies {
        let inner_types = self.types.iter().map(|ty| ty.id).collect();

        let mut normalization_deps = Vec::new();
        for ty_ref in &self.types {
            let Some(ty) = types.get(ty_ref.id) else {
                // Can't normalize, bail
                return TypeDependencies {
                    inner_types,
                    ..Default::default()
                };
            };

            if matches!(ty.kind, TyTypeKind::Union(_)) {
                normalization_deps.push(ty_ref.id);
            }
        }

        TypeDependencies {
            inner_types,
            ..Default::default()
        }
    }
}
