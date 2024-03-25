use std::collections::HashMap;

use crate::lang::solver::{MId, ModItemSet};

use super::TyType;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// An arbitrary type relationship, e.g. if a type is assignable to another, or a type is a subset of another.
pub struct TypeRelationship<T: std::fmt::Debug + Clone + PartialEq + Eq + std::hash::Hash> {
    pub from: MId<TyType>,
    pub to: MId<TyType>,
    _phantom: std::marker::PhantomData<T>,
}

impl<T: std::fmt::Debug + Clone + PartialEq + Eq + std::hash::Hash> TypeRelationship<T> {
    pub fn new(from: MId<TyType>, to: MId<TyType>) -> Self {
        Self {
            from,
            to,
            _phantom: std::marker::PhantomData,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Assignable;

type TypeAssignability = TypeRelationship<Assignable>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SubsetOf;

type TypeSubsetability = TypeRelationship<SubsetOf>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeRelationshipCache<T: std::fmt::Debug + Clone + PartialEq + Eq + std::hash::Hash> {
    cache: HashMap<TypeRelationship<T>, bool>,
}

impl<T: std::fmt::Debug + Clone + PartialEq + Eq + std::hash::Hash> TypeRelationshipCache<T> {
    pub fn new() -> Self {
        Self {
            cache: HashMap::new(),
        }
    }

    pub fn has(&self, from: MId<TyType>, to: MId<TyType>) -> Option<bool> {
        self.cache.get(&TypeRelationship::new(from, to)).copied()
    }

    pub fn set(&mut self, from: MId<TyType>, to: MId<TyType>, assignable: bool) {
        self.cache
            .insert(TypeRelationship::new(from, to), assignable);
    }
}

pub type TypeAssignabilityCache = TypeRelationshipCache<Assignable>;
pub type TypeSubsetabilityCache = TypeRelationshipCache<SubsetOf>;

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
        if let Some(assignable) = self.type_assignability.has(left, right) {
            return assignable;
        }

        let assignable = self.is_assignable_to_impl(left, right);

        self.type_assignability.set(left, right, assignable);

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
    type_subsetability: &'a mut TypeSubsetabilityCache,
    parent_queries: Vec<TypeSubsetability>,
}

impl<'a> TypeSubsetQuery<'a> {
    pub fn new(
        types: &'a ModItemSet<TyType>,
        type_subsetability: &'a mut TypeSubsetabilityCache,
    ) -> Self {
        Self {
            types,
            type_subsetability,
            parent_queries: Vec::new(),
        }
    }

    pub fn is_substet_of(&mut self, left: MId<TyType>, right: MId<TyType>) -> bool {
        if let Some(assignable) = self.type_subsetability.has(left, right) {
            return assignable;
        }

        let assignable = self.is_subset_of_impl(left, right);

        self.type_subsetability.set(left, right, assignable);

        assignable
    }

    pub fn is_subset_of_impl(&mut self, left: MId<TyType>, right: MId<TyType>) -> bool {
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
    types: &'a ModItemSet<TyType>,
    type_assignability: &'a mut TypeAssignabilityCache,
    left: MId<TyType>,
    right: MId<TyType>,
) -> bool {
    let mut query = TypeAssignabilityQuery::new(types, type_assignability);
    query.is_assignable_to(left, right)
}
