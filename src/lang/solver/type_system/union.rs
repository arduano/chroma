use crate::lang::solver::{MId, ModItemSet, TypeAssignabilityQuery, TypeSubsetQuery};

use super::{TyType, TyTypeKind, TyTypeLogic, TyTypeOrBorrowRef, TyTypeOrRef};

#[derive(Debug, Clone)]
pub struct TyUnion {
    /// List of references to union types.
    /// This list can't contain other unions.
    pub types: Vec<MId<TyType>>,
}

fn is_type_subset_of(
    ty: &TyTypeOrBorrowRef,
    ty2: &TyTypeOrBorrowRef,
    types: &ModItemSet<TyType>,
) -> bool {
    let ty = ty.get(types);
    let ty2 = ty2.get(types);
    if !ty.is_substate_of(ty2, &mut TypeSubsetQuery::new(types)) {
        return false;
    }
    true
}

fn is_type_subset_of_union(
    ty: &TyTypeOrBorrowRef,
    union_list: &Vec<MId<TyType>>,
    types: &ModItemSet<TyType>,
) -> bool {
    if union_list.len() == 0 {
        return false;
    }

    let ty = ty.get(types);
    for union_ty in union_list {
        let union_ty = &types[union_ty];
        if !ty.is_substate_of(union_ty, &mut TypeSubsetQuery::new(types)) {
            return false;
        }
    }
    true
}

fn is_type_subset_of_union_with_query(
    ty: &TyTypeOrBorrowRef,
    union_list: &Vec<MId<TyType>>,
    types: &ModItemSet<TyType>,
    subset_query: &mut TypeSubsetQuery,
) -> bool {
    if union_list.len() == 0 {
        return false;
    }

    let ty = ty.get(types);
    for union_ty in union_list {
        let union_ty = &types[union_ty];
        if !ty.is_substate_of(union_ty, subset_query) {
            return false;
        }
    }
    true
}

fn is_type_assignable_to_union(
    ty: &TyTypeOrBorrowRef,
    union_list: &Vec<MId<TyType>>,
    assignability: &mut TypeAssignabilityQuery,
) -> bool {
    if union_list.len() == 0 {
        return false;
    }

    let ty = ty.get(assignability.types);
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

    pub fn new_with_types(types: Vec<MId<TyType>>) -> Self {
        Self { types }
    }

    fn insert_type(&mut self, ty_ref: TyTypeOrRef, types: &mut ModItemSet<TyType>) {
        let ty = ty_ref.get(types);

        if let TyTypeKind::Union(union) = &ty.kind {
            // Cloning here to keep the borrow checker happy
            let union_types_cloned = union.types.clone();
            for &ty in &union_types_cloned {
                self.insert_type(TyTypeOrRef::Ref(ty), types);
            }
            return;
        }

        // If it's already a substate, ignore
        if is_type_subset_of_union(&ty_ref.to_borrowed(), &self.types, types) {
            return;
        }

        // If it's a superset of any of the types, remove the types
        let mut i = 0;
        while i < self.types.len() {
            if is_type_subset_of(
                &TyTypeOrBorrowRef::Ref(self.types[i]),
                &ty_ref.to_borrowed(),
                types,
            ) {
                self.types.remove(i);
            } else {
                i += 1;
            }
        }

        // Insert the type
        self.types.push(ty_ref.to_id(types));
    }

    pub fn join_unions(left: &Self, right: &Self, types: &mut ModItemSet<TyType>) -> TyTypeKind {
        let mut new_union = TyUnion::new();
        for &ty in &left.types {
            new_union.insert_type(TyTypeOrRef::Ref(ty), types);
        }
        for &ty in &right.types {
            new_union.insert_type(TyTypeOrRef::Ref(ty), types);
        }

        if new_union.types.len() == 1 {
            TyTypeKind::Reference(new_union.types[0])
        } else {
            TyTypeKind::Union(new_union)
        }
    }

    pub fn union_types(
        left: TyTypeOrRef,
        right: TyTypeOrRef,
        types: &mut ModItemSet<TyType>,
    ) -> TyTypeKind {
        let mut new_union = TyUnion::new();
        new_union.insert_type(left, types);
        new_union.insert_type(right, types);

        if new_union.types.len() == 1 {
            // TODO: Avoid reference indirection if possible
            TyTypeKind::Reference(new_union.types[0])
        } else {
            TyTypeKind::Union(new_union)
        }
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
            if is_type_assignable_to_union(&TyTypeOrBorrowRef::Borrowed(ty), &other.types, query) {
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
            if is_type_subset_of_union_with_query(
                &TyTypeOrBorrowRef::Borrowed(ty),
                &other.types,
                query.types,
                query,
            ) {
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
