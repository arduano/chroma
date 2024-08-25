use std::collections::HashMap;

use crate::lang::solver_old::{MId, ModItemSet, TypeData};

use super::{TyType, TypeId};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// An arbitrary type relationship, e.g. if a type is assignable to another, or a type is a subset of another.
pub struct TypeRelationship<T: std::fmt::Debug + Clone + PartialEq + Eq + std::hash::Hash> {
    pub from: TypeId,
    pub to: TypeId,
    _phantom: std::marker::PhantomData<T>,
}

impl<T: std::fmt::Debug + Clone + PartialEq + Eq + std::hash::Hash> TypeRelationship<T> {
    pub fn new(from: TypeId, to: TypeId) -> Self {
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

    pub fn has(&self, from: TypeId, to: TypeId) -> Option<bool> {
        self.cache.get(&TypeRelationship::new(from, to)).copied()
    }

    pub fn set(&mut self, from: TypeId, to: TypeId, assignable: bool) {
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

    pub fn is_type_assignable_to_type(&mut self, left: TypeId, right: TypeId) -> bool {
        if let Some(assignable) = self.type_assignability.has(left, right) {
            return assignable;
        }

        let assignable = self.is_assignable_to_impl(left, right);

        self.type_assignability.set(left, right, assignable);

        assignable
    }

    fn is_assignable_to_impl(&mut self, left: TypeId, right: TypeId) -> bool {
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

    pub fn is_type_subtype_of_type(&mut self, left: TypeId, right: TypeId) -> bool {
        if let Some(assignable) = self.type_subsetability.has(left, right) {
            return assignable;
        }

        let assignable = self.is_subset_of_impl(left, right);

        self.type_subsetability.set(left, right, assignable);

        assignable
    }

    pub fn is_subset_of_impl(&mut self, left: TypeId, right: TypeId) -> bool {
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

pub struct TypeRelationships {
    pub type_assignability: TypeAssignabilityCache,
    pub type_subsetability: TypeSubsetabilityCache,
}

impl TypeRelationships {
    pub fn new() -> Self {
        Self {
            type_assignability: TypeAssignabilityCache::new(),
            type_subsetability: TypeSubsetabilityCache::new(),
        }
    }
}

impl TypeRelationships {
    pub fn is_type_assignable_to_type(
        &mut self,
        types: &TypeData,
        left: TypeId,
        right: TypeId,
    ) -> bool {
        let mut query = TypeAssignabilityQuery::new(&types, &mut self.type_assignability);
        query.is_type_assignable_to_type(left, right)
    }

    pub fn is_type_subtype_of_type(
        &mut self,
        types: &TypeData,
        left: TypeId,
        right: TypeId,
    ) -> bool {
        let mut query = TypeSubsetQuery::new(&types, &mut self.type_subsetability);
        query.is_type_subtype_of_type(left, right)
    }
}
