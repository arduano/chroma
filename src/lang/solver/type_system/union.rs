use crate::lang::{
    solver::{
        ModItemSet, TyIdOrValWithSpan, TypeAssignabilityQuery, TypeIdWithSpan, TypeSubsetQuery,
    },
    tokens::{Span, TkIdent},
    CompilerError, ErrorCollector,
};

use super::{TyType, TyTypeKind, TyTypeLogic};

#[derive(Debug, Clone)]
pub struct TyUnion {
    /// List of references to union types.
    /// This list can't contain other unions.
    pub types: Vec<TypeIdWithSpan>,
}

fn is_type_subset_of(ty: &TyType, ty2: &TyType, types: &ModItemSet<TyType>) -> bool {
    if !ty.is_substate_of(ty2, &mut TypeSubsetQuery::new(types)) {
        return false;
    }
    true
}

fn is_type_subset_of_union(
    ty: &TyType,
    union_list: &Vec<TypeIdWithSpan>,
    types: &ModItemSet<TyType>,
) -> bool {
    if union_list.len() == 0 {
        return false;
    }

    for union_ty in union_list {
        let union_ty = &types[union_ty.id];
        if !ty.is_substate_of(union_ty, &mut TypeSubsetQuery::new(types)) {
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

    pub fn insert_type(
        &mut self,
        ty_ref: TyIdOrValWithSpan,
        types: &mut ModItemSet<TyType>,
        errors: &mut ErrorCollector,
    ) {
        let ty = types.get_val_for_val_or_id(&ty_ref.ty);

        let Some(ty) = ty else {
            // Recursive expression found, illegal
            errors.push(CompilerError::new("Recursive type union", ty_ref.span));
            return;
        };

        if let TyTypeKind::Union(union) = &ty.kind {
            // Cloning here to keep the borrow checker happy
            let union_types_cloned = union.types.clone();
            for ty in &union_types_cloned {
                self.insert_type(ty.as_type_id_or_val(), types, errors);
            }
            return;
        }

        // If it's already a substate, ignore
        if is_type_subset_of_union(&ty, &self.types, types) {
            return;
        }

        // If it's a superset of any of the types, remove the types
        let mut i = 0;
        while i < self.types.len() {
            let union_ty = &types.get(self.types[i].id);

            let Some(union_ty) = union_ty else {
                // Recursive expression found, illegal
                errors.push(CompilerError::new(
                    "Recursive type union",
                    ty_ref.span.clone(),
                ));
                i += 1;
                continue;
            };

            if is_type_subset_of(&union_ty, &ty, types) {
                self.types.remove(i);
            } else {
                i += 1;
            }
        }

        let id = types.get_id_for_val_or_id(ty_ref.ty);

        // Insert the type
        self.types.push(TypeIdWithSpan::new(id, ty_ref.span));
    }

    pub fn union_types(
        left: TyIdOrValWithSpan,
        right: TyIdOrValWithSpan,
        span: Span,
        name: Option<TkIdent>,
        types: &mut ModItemSet<TyType>,
        errors: &mut ErrorCollector,
    ) -> TyIdOrValWithSpan {
        let mut new_union = TyUnion::new();

        new_union.insert_type(left, types, errors);
        new_union.insert_type(right, types, errors);

        if new_union.types.len() == 1 {
            let ty = &new_union.types[0];
            TyIdOrValWithSpan::new_id(ty.id, ty.span.clone())
        } else {
            let kind = TyTypeKind::Union(new_union);
            let ty = TyType::new_named(name, kind, span.clone());
            TyIdOrValWithSpan::new_val(ty, span)
        }
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
}
