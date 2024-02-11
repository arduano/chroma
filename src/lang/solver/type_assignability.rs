use std::collections::HashMap;

use super::{type_system::TyType, MId, ModItemSet, ModuleGroupCompilation};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeAssignability {
    pub from: MId<TyType>,
    pub to: MId<TyType>,
}

impl TypeAssignability {
    pub fn new(from: MId<TyType>, to: MId<TyType>) -> Self {
        Self { from, to }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeSubsetability {
    pub from: MId<TyType>,
    pub to: MId<TyType>,
}

impl TypeSubsetability {
    pub fn new(from: MId<TyType>, to: MId<TyType>) -> Self {
        Self { from, to }
    }
}

pub struct TypeAssignabilityCache {
    assignability_cache: HashMap<TypeAssignability, bool>,
}

impl TypeAssignabilityCache {
    pub fn new() -> Self {
        Self {
            assignability_cache: HashMap::new(),
        }
    }

    pub fn get_assignable_to_cache(&self, from: MId<TyType>, to: MId<TyType>) -> Option<bool> {
        self.assignability_cache
            .get(&TypeAssignability::new(from, to))
            .copied()
    }

    pub fn set_assignable_to(&mut self, from: MId<TyType>, to: MId<TyType>, assignable: bool) {
        self.assignability_cache
            .insert(TypeAssignability::new(from, to), assignable);
    }
}

pub struct TypeAssignabilityQuery<'a> {
    pub types: &'a ModItemSet<TyType>,
    type_assignability: &'a mut TypeAssignabilityCache,
    parent_queries: Vec<TypeAssignability>,
}

impl<'a> TypeAssignabilityQuery<'a> {
    pub fn new(
        types: &'a ModItemSet<TyType>,
        type_assignability: &'a mut TypeAssignabilityCache,
    ) -> Self {
        Self {
            types,
            type_assignability,
            parent_queries: Vec::new(),
        }
    }

    pub fn is_assignable_to(&mut self, left: MId<TyType>, right: MId<TyType>) -> bool {
        if let Some(assignable) = self.type_assignability.get_assignable_to_cache(left, right) {
            return assignable;
        }

        let assignable = self.is_assignable_to_impl(left, right);

        self.type_assignability
            .set_assignable_to(left, right, assignable);

        assignable
    }

    fn is_assignable_to_impl(&mut self, left: MId<TyType>, right: MId<TyType>) -> bool {
        if left == right {
            return true;
        }

        if self
            .parent_queries
            .contains(&TypeAssignability::new(left, right))
        {
            return true;
        }

        self.parent_queries
            .push(TypeAssignability::new(left, right));

        let left_ty = &self.types[&left];
        let right_ty = &self.types[&right];

        let assignable = left_ty.check_assignable_to(right_ty, self);

        self.parent_queries.pop();

        assignable
    }
}

pub struct TypeSubsetQuery<'a> {
    pub types: &'a ModItemSet<TyType>,
    parent_queries: Vec<TypeSubsetability>,
}

impl<'a> TypeSubsetQuery<'a> {
    pub fn new(types: &'a ModItemSet<TyType>) -> Self {
        Self {
            types,
            parent_queries: Vec::new(),
        }
    }

    pub fn is_substate_of(&mut self, left: MId<TyType>, right: MId<TyType>) -> bool {
        if left == right {
            return true;
        }

        if self
            .parent_queries
            .contains(&TypeSubsetability::new(left, right))
        {
            return true;
        }

        self.parent_queries
            .push(TypeSubsetability::new(left, right));

        let left_ty = &self.types[&left];
        let right_ty = &self.types[&right];

        let assignable = left_ty.is_substate_of(right_ty, self);

        self.parent_queries.pop();

        assignable
    }
}

pub fn run_type_assignability_query<'a>(
    compilation: &'a mut ModuleGroupCompilation,
    left: MId<TyType>,
    right: MId<TyType>,
) -> bool {
    let mut query =
        TypeAssignabilityQuery::new(&compilation.types, &mut compilation.type_assignability);
    query.is_assignable_to(left, right)
}
