use std::{collections::HashMap, marker::PhantomData, sync::Arc};

use frunk::labelled::chars::R;

use super::{
    options_equal_or_true, ordered_set::OrderedSet, Ty2FieldSelect, Ty2SystemStorage, Ty2TypeId,
    Ty2TypeVariant, Ty2TypeVariantKind, Ty2VariantId,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypePair {
    left: Ty2TypeId,
    right: Ty2TypeId,
}

impl TypePair {
    pub fn new(left: Ty2TypeId, right: Ty2TypeId) -> Self {
        Self { left, right }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeVariantPair {
    left: Ty2VariantId,
    right: Ty2VariantId,
}

impl TypeVariantPair {
    pub fn new(left: Ty2VariantId, right: Ty2VariantId) -> Self {
        Self { left, right }
    }
}

pub struct TypeRelationshipCache<Result: Copy> {
    type_cache: HashMap<TypePair, Result>,
    variant_cache: HashMap<TypeVariantPair, Result>,
}

impl<Result: Copy> TypeRelationshipCache<Result> {
    pub fn new() -> Self {
        Self {
            type_cache: HashMap::new(),
            variant_cache: HashMap::new(),
        }
    }

    pub fn get_type_cache(&self, left: Ty2TypeId, right: Ty2TypeId) -> Option<Result> {
        let pair = TypePair::new(left, right);
        self.type_cache.get(&pair).copied()
    }

    pub fn get_variant_cache(&self, left: Ty2VariantId, right: Ty2VariantId) -> Option<Result> {
        let pair = TypeVariantPair::new(left, right);
        self.variant_cache.get(&pair).copied()
    }

    pub fn add_type_cache(&mut self, left: Ty2TypeId, right: Ty2TypeId, result: Result) {
        let pair = TypePair::new(left, right);
        self.type_cache.insert(pair, result);
    }

    pub fn add_variant_cache(&mut self, left: Ty2VariantId, right: Ty2VariantId, result: Result) {
        let pair = TypeVariantPair::new(left, right);
        self.variant_cache.insert(pair, result);
    }
}

macro_rules! impl_relationship_probe {
    ($name:ident, $union_fn:ident, $ty_fn:ident) => {
        pub struct $name<'a> {
            cache: &'a mut TypeRelationshipCache<bool>,
            prev_type_queries: Vec<TypePair>,
            prev_variant_queries: Vec<TypeVariantPair>,
        }

        impl<'a> $name<'a> {
            pub fn new(cache: &'a mut TypeRelationshipCache<bool>) -> Self {
                Self {
                    cache,
                    prev_type_queries: Vec::new(),
                    prev_variant_queries: Vec::new(),
                }
            }

            pub fn query_for_type(
                &mut self,
                types: &Ty2SystemStorage,
                left: Ty2TypeId,
                right: Ty2TypeId,
            ) -> bool {
                if left == right {
                    return true;
                }

                let pair = TypePair::new(left, right);
                if self.prev_type_queries.contains(&pair) {
                    return true;
                }

                if let Some(result) = self.cache.get_type_cache(left, right) {
                    return result;
                }

                self.prev_type_queries.push(pair);
                let assignable = $union_fn(types, left, right, |left, right| {
                    self.query_for_variant(types, left, right)
                });
                self.prev_type_queries.pop();

                self.cache.add_type_cache(left, right, assignable);

                assignable
            }

            pub fn query_for_variant(
                &mut self,
                types: &Ty2SystemStorage,
                left: Ty2VariantId,
                right: Ty2VariantId,
            ) -> bool {
                if left == right {
                    return true;
                }

                let pair = TypeVariantPair::new(left, right);
                if self.prev_variant_queries.contains(&pair) {
                    return true;
                }

                if let Some(result) = self.cache.get_variant_cache(left, right) {
                    return result;
                }

                self.prev_variant_queries.push(pair);
                let assignable = $ty_fn(types, left, right, |left, right| {
                    self.query_for_type(types, left, right)
                });
                self.prev_variant_queries.pop();

                self.cache.add_variant_cache(left, right, assignable);

                assignable
            }
        }
    };
}

fn check_type_union_assignability_respecting_backing_type(
    system_storage: &Ty2SystemStorage,
    left: Ty2TypeId,
    right: Ty2TypeId,
    mut callback: impl FnMut(Ty2VariantId, Ty2VariantId) -> bool,
) -> bool {
    // TODO: No unwrap
    let left_ty = system_storage.types.get(left).unwrap();
    let right_ty = system_storage.types.get(right).unwrap();

    // If the right type has no variants, nothing can be assigned to it
    if right_ty.variants.len() == 0 {
        return false;
    }

    if !options_equal_or_true(left_ty.backing, right_ty.backing) {
        return false;
    }

    for left_variant in left_ty.variants.iter() {
        let true_for_any = right_ty.variants.iter().any(|right_variant| {
            options_equal_or_true(left_variant.backing_id, right_variant.backing_id)
                && callback(left_variant.id, right_variant.id)
        });

        if !true_for_any {
            return false;
        }
    }

    true
}

fn check_type_union_assignability_without_backing_type(
    system_storage: &Ty2SystemStorage,
    left: Ty2TypeId,
    right: Ty2TypeId,
    mut callback: impl FnMut(Ty2VariantId, Ty2VariantId) -> bool,
) -> bool {
    // TODO: No unwrap
    let left_ty = system_storage.types.get(left).unwrap();
    let right_ty = system_storage.types.get(right).unwrap();

    // If the right type has no variants, nothing can be assigned to it
    if right_ty.variants.len() == 0 {
        return false;
    }

    for left_variant in left_ty.variants.iter() {
        let true_for_any = right_ty
            .variants
            .iter()
            .any(|right_variant| callback(left_variant.id, right_variant.id));

        if !true_for_any {
            return false;
        }
    }

    true
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum ComparisonKind {
    Assignable,
    Subset,
    Equal,
}

fn check_type_variant_assignability_respecting_backing_type(
    system_storage: &Ty2SystemStorage,
    left: Ty2VariantId,
    right: Ty2VariantId,
    mut callback: impl FnMut(Ty2TypeId, Ty2TypeId) -> bool,
) -> bool {
    compare_two_types(
        system_storage,
        left,
        right,
        ComparisonKind::Assignable,
        callback,
    )
}

fn check_type_variant_subset_respecting_backing_type(
    system_storage: &Ty2SystemStorage,
    left: Ty2VariantId,
    right: Ty2VariantId,
    mut callback: impl FnMut(Ty2TypeId, Ty2TypeId) -> bool,
) -> bool {
    compare_two_types(
        system_storage,
        left,
        right,
        ComparisonKind::Subset,
        callback,
    )
}

fn check_type_variant_equality(
    system_storage: &Ty2SystemStorage,
    left: Ty2VariantId,
    right: Ty2VariantId,
    callback: impl FnMut(Ty2TypeId, Ty2TypeId) -> bool,
) -> bool {
    compare_two_types(system_storage, left, right, ComparisonKind::Equal, callback)
}

fn compare_two_types(
    system_storage: &Ty2SystemStorage,
    left: Ty2VariantId,
    right: Ty2VariantId,
    kind: ComparisonKind,
    callback: impl FnMut(Ty2TypeId, Ty2TypeId) -> bool,
) -> bool {
    // TODO: No unwrap
    let left_ty = system_storage.type_variants.get(left).unwrap();
    let right_ty = system_storage.type_variants.get(right).unwrap();

    use ComparisonKind as Kind;
    use Ty2TypeVariantKind::*;

    match (&left_ty.kind, &right_ty.kind, kind) {
        // Any is equal to any
        (Any, Any, Kind::Equal) => true,
        // Anything is assignable/subset to any
        (_, Any, _) => true,
        // Any is not assignable/subset/equal to anything else
        (Any, _, _) => false,

        // Strings are equal if they are the same
        (String(str1), String(str2), Kind::Equal) => str1 == str2,
        // Strings may be assignable/subset to other strings
        (String(str1), String(str2), _) => {
            match (str1, str2) {
                // Any string is assignable to a non-literal string
                (_, None) => true,

                // Non-literal strings are not assignable to literal strings
                (None, Some(_)) => false,

                // Literal strings are assignable to literal strings if they are the same
                (Some(str1), Some(str2)) => str1 == str2,
            }
        }
        // Strings aren't assignable/subset/equal to anything else
        (String(_), _, _) => false,

        // Numbers are equal if they are the same
        (Number(num1), Number(num2), Kind::Equal) => num1 == num2,
        // Numbers may be assignable/subset to other numbers
        (Number(num1), Number(num2), _) => {
            match (num1, num2) {
                // Any number is assignable to a non-literal number
                (_, None) => true,

                // Non-literal numbers are not assignable to literal numbers
                (None, Some(_)) => false,

                // Literal numbers are assignable to literal numbers if they are the same
                (Some(num1), Some(num2)) => num1 == num2,
            }
        }
        // Numbers aren't assignable/subset/equal to anything else
        (Number(_), _, _) => false,

        // True is assignable/subset/equal to true
        (True, True, _) => true,
        // True is not assignable/subset/equal to anything else
        (True, _, _) => false,

        // False is assignable/subset/equal to false
        (False, False, _) => true,
        // False is not assignable/subset/equal to anything else
        (False, _, _) => false,

        // AttribSets may be assignable to other AttribSets
        (AttribSet(set1), AttribSet(set2), Kind::Assignable) => {
            is_attib_set_assignable(set1, set2, callback)
        }
        // AttribSets may be subset/equal to other AttribSets
        (AttribSet(set1), AttribSet(set2), Kind::Subset)
        | (AttribSet(set1), AttribSet(set2), Kind::Equal) => {
            is_attrib_set_subset_or_equal(set1, set2, callback)
        }
        // AttribSets aren't assignable/subset/equal to anything else
        (AttribSet(_), _, _) => false,
    }
}

fn is_attib_set_assignable(
    left: &OrderedSet<Arc<str>, Ty2FieldSelect>,
    right: &OrderedSet<Arc<str>, Ty2FieldSelect>,
    mut callback: impl FnMut(Ty2TypeId, Ty2TypeId) -> bool,
) -> bool {
    // Every key on the left has a matching key on the right
    for (key1, value1) in left.iter() {
        let Some(value2) = right.get(key1) else {
            return false;
        };

        if value1.kind != value2.kind {
            return false;
        }

        if !callback(value1.id, value2.id) {
            return false;
        }
    }

    true
}

fn is_attrib_set_subset_or_equal(
    left: &OrderedSet<Arc<str>, Ty2FieldSelect>,
    right: &OrderedSet<Arc<str>, Ty2FieldSelect>,
    mut callback: impl FnMut(Ty2TypeId, Ty2TypeId) -> bool,
) -> bool {
    // The number of keys is the same
    if left.len() != right.len() {
        return false;
    }

    // Every key on the left has a matching key on the right
    for (key1, value1) in left.iter() {
        let Some(value2) = right.get(key1) else {
            return false;
        };

        if value1.kind != value2.kind {
            return false;
        }

        if !callback(value1.id, value2.id) {
            return false;
        }
    }

    true
}

impl_relationship_probe!(
    TypeAssignabilityProbe,
    check_type_union_assignability_respecting_backing_type,
    check_type_variant_assignability_respecting_backing_type
);

impl_relationship_probe!(
    TypeSubsetProbe,
    check_type_union_assignability_respecting_backing_type,
    check_type_variant_subset_respecting_backing_type
);

impl_relationship_probe!(
    TypeEqualityProbe,
    check_type_union_assignability_respecting_backing_type,
    check_type_variant_equality
);

struct TypeRecursionProbe<'a> {
    cache: &'a mut HashMap<Ty2TypeId, bool>,
    stack: Vec<Ty2TypeId>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Unresolved;

impl<'a> TypeRecursionProbe<'a> {
    pub fn new(cache: &'a mut HashMap<Ty2TypeId, bool>) -> Self {
        Self {
            cache,
            stack: Vec::new(),
        }
    }

    pub fn query_for_type(
        &mut self,
        types: &Ty2SystemStorage,
        ty: Ty2TypeId,
    ) -> Result<bool, Unresolved> {
        if let Some(result) = self.cache.get(&ty) {
            return Ok(*result);
        }

        if self.stack.contains(&ty) {
            self.cache.insert(ty, true);
            return Ok(true);
        }

        self.stack.push(ty);

        let result = self.run_for_child_types(types, ty);

        self.stack.pop();

        if let Ok(result) = result {
            self.cache.insert(ty, result);
            return Ok(result);
        } else {
            return Err(Unresolved);
        }
    }

    fn run_for_child_types(
        &mut self,
        types: &Ty2SystemStorage,
        ty: Ty2TypeId,
    ) -> Result<bool, Unresolved> {
        let ty = types.types.get(ty).ok_or(Unresolved)?;

        let mut has_unresolved = false;
        macro_rules! process_result {
            ($result:expr) => {
                if $result as Result<bool, Unresolved> == Err(Unresolved) {
                    has_unresolved = true;
                } else if $result == Ok(true) {
                    return Ok(true);
                }
            };
        }

        for variant in ty.variants.iter() {
            let variant = types.type_variants.get(variant.id);
            let Some(variant) = variant else {
                process_result!(Err(Unresolved));
                continue;
            };

            match &variant.kind {
                Ty2TypeVariantKind::Any
                | Ty2TypeVariantKind::String(_)
                | Ty2TypeVariantKind::Number(_)
                | Ty2TypeVariantKind::True
                | Ty2TypeVariantKind::False => {}

                Ty2TypeVariantKind::AttribSet(set) => {
                    for (_, value) in set.iter() {
                        let key_result = self.query_for_type(types, value.id);
                        process_result!(key_result);
                    }
                }
            }
        }

        if has_unresolved {
            Err(Unresolved)
        } else {
            Ok(false)
        }
    }
}

pub struct TypeRelationships {
    pub type_assignability: TypeRelationshipCache<bool>,
    pub type_subset: TypeRelationshipCache<bool>,
    pub type_equality: TypeRelationshipCache<bool>,
    pub is_recursive_type: HashMap<Ty2TypeId, bool>,
}

impl TypeRelationships {
    pub fn new() -> Self {
        Self {
            type_assignability: TypeRelationshipCache::new(),
            type_subset: TypeRelationshipCache::new(),
            type_equality: TypeRelationshipCache::new(),
            is_recursive_type: HashMap::new(),
        }
    }

    pub fn is_type_assignable_to_type(
        &mut self,
        types: &Ty2SystemStorage,
        left: Ty2TypeId,
        right: Ty2TypeId,
    ) -> bool {
        let mut probe = TypeAssignabilityProbe::new(&mut self.type_assignability);
        probe.query_for_type(types, left, right)
    }

    pub fn is_type_subset_of_type(
        &mut self,
        types: &Ty2SystemStorage,
        left: Ty2TypeId,
        right: Ty2TypeId,
    ) -> bool {
        let mut probe = TypeSubsetProbe::new(&mut self.type_subset);
        probe.query_for_type(types, left, right)
    }

    pub fn is_type_equal_to_type(
        &mut self,
        types: &Ty2SystemStorage,
        left: Ty2TypeId,
        right: Ty2TypeId,
    ) -> bool {
        let mut probe = TypeEqualityProbe::new(&mut self.type_equality);
        probe.query_for_type(types, left, right)
    }

    /// Returns true if the type is recursive, false if it is not, and Err if no recursion was found but it's also not a fully resolved type.
    pub fn is_recursive_type(
        &mut self,
        types: &Ty2SystemStorage,
        ty: Ty2TypeId,
    ) -> Result<bool, Unresolved> {
        let mut probe = TypeRecursionProbe::new(&mut self.is_recursive_type);
        probe.query_for_type(types, ty)
    }
}
