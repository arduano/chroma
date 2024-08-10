use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    marker::PhantomData,
};

use frunk::labelled::chars::f;

pub enum TypeLife {
    Temporary,
    Permanent,
}

pub trait TypeStorage {
    type Type;
    type TypeId: Copy + Eq + std::hash::Hash + std::fmt::Debug;

    fn get_type(&self, id: Self::TypeId) -> &Self::Type;

    fn insert_type(&self, ty: Self::Type, life: TypeLife) -> Self::TypeId;

    fn insert_permanent_type(&mut self, ty: Self::Type) -> Self::TypeId {
        self.insert_type(ty, TypeLife::Permanent)
    }
    fn insert_temporary_type(&mut self, ty: Self::Type) -> Self::TypeId {
        self.insert_type(ty, TypeLife::Temporary)
    }

    fn make_type_permanent(&mut self, id: Self::TypeId) -> Self::TypeId;
}

pub trait TypeValidation {
    type TypeStorage: TypeStorage;
    type TypeId: Copy + Eq + std::hash::Hash + std::fmt::Debug;

    fn are_types_equal(
        &mut self,
        storage: &Self::TypeStorage,
        left: Self::TypeId,
        right: Self::TypeId,
    ) -> bool;
    fn is_type_assignable_to(
        &mut self,
        storage: &Self::TypeStorage,
        left: Self::TypeId,
        right: Self::TypeId,
    ) -> bool;
    fn is_type_subtype_of(
        &mut self,
        storage: &Self::TypeStorage,
        left: Self::TypeId,
        right: Self::TypeId,
    ) -> bool;

    fn make_union(
        &mut self,
        storage: &Self::TypeStorage,
        of: impl Iterator<Item = Self::TypeId>,
        life: TypeLife,
    ) -> Self::TypeId;
    fn make_permanent_union(
        &mut self,
        storage: &Self::TypeStorage,
        of: impl Iterator<Item = Self::TypeId>,
    ) -> Self::TypeId {
        self.make_union(storage, of, TypeLife::Permanent)
    }
    fn make_temporary_union(
        &mut self,
        storage: &Self::TypeStorage,
        of: impl Iterator<Item = Self::TypeId>,
    ) -> Self::TypeId {
        self.make_union(storage, of, TypeLife::Temporary)
    }
}

pub trait UnionType {
    type InnerTypeIds: Copy + Eq + std::hash::Hash + std::fmt::Debug;
    fn variants(&self) -> Cow<'_, [Self::InnerTypeIds]>;
    fn new_non_normalized(variants: Vec<Self::InnerTypeIds>) -> Self;
}

pub trait AbstractType {
    type Metadata: Clone;

    fn get_metadata(&self) -> &Self::Metadata;
}

pub trait IsType<T>: AbstractType {
    fn as_type(&self) -> Option<&T>;
    fn is_type(&self) -> bool {
        self.as_type().is_some()
    }

    fn from_type(ty: T, metadata: Self::Metadata) -> Self;
}

pub trait ContainerType {
    type Key: Clone + Eq + std::fmt::Debug;
    type Value: std::fmt::Debug;

    fn get(&self, key: &Self::Key) -> Option<&Self::Value>;

    fn keys(&self) -> impl '_ + Iterator<Item = &Self::Key>;
    fn iter(&self) -> impl '_ + Iterator<Item = (&Self::Key, &Self::Value)>;
    fn values(&self) -> impl '_ + Iterator<Item = &Self::Value>;
    fn len(&self) -> usize;

    fn new(pairs: impl Iterator<Item = (Self::Key, Self::Value)>) -> Self;
}

struct UnionShrink<Container: ContainerType>(PhantomData<Container>);

impl<Container: ContainerType> UnionShrink<Container> {
    pub fn shrink<Storage, Validation, StorageType, Union>(
        storage: &mut Storage,
        validation: &mut Validation,
        union: &Union,
    ) -> Union
    where
        Container::Value: Copy + Eq + std::hash::Hash + std::fmt::Debug + Ord,
        Storage: TypeStorage<Type = StorageType, TypeId = Container::Value>,
        Validation: TypeValidation<TypeStorage = Storage, TypeId = Storage::TypeId>,
        StorageType: IsType<Container> + Clone,
        Union: UnionType<InnerTypeIds = Storage::TypeId>,
    {
        UnionShrinker::shrink(storage, validation, union)
    }
}

struct UnionShrinker<TypeId, Container, Storage, Validation, StorageType, Union>(
    PhantomData<(TypeId, Container, Storage, Validation, StorageType, Union)>,
);

impl<TypeId, Container, Storage, Validation, StorageType, Union>
    UnionShrinker<TypeId, Container, Storage, Validation, StorageType, Union>
where
    TypeId: Copy + Eq + std::hash::Hash + std::fmt::Debug + Ord,
    Container: ContainerType<Value = TypeId>,
    Storage: TypeStorage<Type = StorageType, TypeId = TypeId>,
    Validation: TypeValidation<TypeStorage = Storage, TypeId = TypeId>,
    StorageType: IsType<Container> + Clone,
    Union: UnionType<InnerTypeIds = Storage::TypeId>,
{
    fn shrink(storage: &mut Storage, validation: &mut Validation, union: &Union) -> Union {
        let mut variants = union.variants().into_owned();

        let mut already_compared = HashSet::new();

        'outer: loop {
            for i in 0..variants.len() {
                let left_id = variants[i];
                let left_abstract = storage.get_type(left_id);
                let Some(left) = left_abstract.as_type() else {
                    continue;
                };

                for j in (i + 1)..variants.len() {
                    let right_id = variants[j];
                    let right_abstract = storage.get_type(right_id);

                    let Some(right) = right_abstract.as_type() else {
                        continue;
                    };

                    let hash_key = (left_id, right_id);
                    if already_compared.contains(&hash_key) {
                        continue;
                    }
                    already_compared.insert(hash_key);

                    if Self::can_join(storage, validation, left, right) {
                        // We can definitely join these types. Make a clone of them so rust doesn't complain
                        let combined = Self::combine_types(storage, validation, left, right);
                        variants.remove(j);
                        variants.remove(i);

                        let combined_abstract =
                            StorageType::from_type(combined, left_abstract.get_metadata().clone());
                        let combined_id = storage.insert_temporary_type(combined_abstract);
                        variants.push(combined_id);

                        continue 'outer;
                    }
                }
            }

            // Successfully reached here without finding a difference
            break;
        }

        Union::new_non_normalized(variants)
    }

    // We assume that the keys are identical
    fn combine_types(
        storage: &Storage,
        validation: &mut Validation,
        left: &Container,
        right: &Container,
    ) -> Container {
        debug_assert_eq!(left.len(), right.len());

        let left_keys = left.keys();
        let key_value_pairs = left_keys.map(|key| {
            let left_value = *left.get(key).unwrap();
            let right_value = *right.get(key).unwrap();

            let unioned =
                validation.make_temporary_union(storage, [left_value, right_value].into_iter());

            (key.clone(), unioned)
        });

        let new_container = Container::new(key_value_pairs);

        new_container
    }

    // Conditions for joining two containers:
    // 1. They have equal keys
    // 2. There is no more than 1 difference between the values of the keys
    fn can_join(
        storage: &Storage,
        validation: &mut Validation,
        left: &Container,
        right: &Container,
    ) -> bool {
        if left.len() != right.len() {
            return false;
        }

        let mut differences = 0;
        for (key, left_value) in left.iter() {
            let Some(right_value) = right.get(key) else {
                return false;
            };

            if !validation.are_types_equal(storage, *left_value, *right_value) {
                differences += 1;
            }

            if differences > 1 {
                return false;
            }
        }

        true
    }
}

//
// TEST IMPLEMENTATION
//

type MockTypeId = usize;

#[derive(Debug, Clone, PartialEq, Eq)]
struct MockTypeStorage {
    types: boxcar::Vec<MockType>,
}

impl MockTypeStorage {
    fn new() -> Self {
        Self {
            types: boxcar::Vec::new(),
        }
    }

    fn from_flat_type(&mut self, ty: MockTypeFlat) -> MockTypeId {
        let inner = match ty {
            MockTypeFlat::Other(inner) => MockType::Other(MockOtherType(inner.0)),
            MockTypeFlat::Object(inner) => MockType::Object(MockObjectType::new(
                inner
                    .entries
                    .into_iter()
                    .map(|(k, v)| (k, self.from_flat_type(v)))
                    .collect(),
            )),
            MockTypeFlat::Union(inner) => MockType::Union(MockUnionType::new(
                inner
                    .variants
                    .into_iter()
                    .map(|v| self.from_flat_type(v))
                    .collect(),
            )),
        };

        let id = self.types.push(inner);

        id
    }

    fn to_flat_type(&self, id: MockTypeId) -> MockTypeFlat {
        let ty = self.types.get(id).unwrap();

        match ty {
            MockType::Other(inner) => MockTypeFlat::Other(MockOtherTypeFlat(inner.0)),
            MockType::Object(inner) => MockTypeFlat::Object(MockObjectTypeFlat::new(
                inner
                    .entries
                    .iter()
                    .map(|(k, v)| (*k, self.to_flat_type(*v)))
                    .collect(),
            )),
            MockType::Union(inner) => MockTypeFlat::Union(MockUnionTypeFlat::new(
                inner
                    .variants
                    .iter()
                    .map(|v| self.to_flat_type(*v))
                    .collect(),
            )),
        }
    }

    fn from_flat_union(&mut self, ty: MockUnionTypeFlat) -> MockUnionType {
        MockUnionType::new(
            ty.variants
                .into_iter()
                .map(|v| self.from_flat_type(v))
                .collect(),
        )
    }

    fn to_flat_union(&self, ty: MockUnionType) -> MockUnionTypeFlat {
        MockUnionTypeFlat::new(ty.variants.iter().map(|v| self.to_flat_type(*v)).collect())
    }
}

impl TypeStorage for MockTypeStorage {
    type Type = MockType;
    type TypeId = MockTypeId;

    fn get_type(&self, id: Self::TypeId) -> &Self::Type {
        self.types.get(id).unwrap()
    }

    fn insert_type(&self, ty: Self::Type, life: TypeLife) -> Self::TypeId {
        self.types.push(ty)
    }

    fn make_type_permanent(&mut self, id: Self::TypeId) -> Self::TypeId {
        // N/A
        id
    }
}

struct MockTypeValidation {}

impl MockTypeValidation {
    fn new() -> Self {
        Self {}
    }
}

impl TypeValidation for MockTypeValidation {
    type TypeStorage = MockTypeStorage;
    type TypeId = MockTypeId;

    fn are_types_equal(
        &mut self,
        storage: &Self::TypeStorage,
        left: Self::TypeId,
        right: Self::TypeId,
    ) -> bool {
        let left_ty = storage.get_type(left);
        let right_ty = storage.get_type(right);

        match (left_ty, right_ty) {
            (MockType::Union(left), MockType::Union(right)) => {
                let mut zipped = left.variants.iter().zip(right.variants.iter());
                zipped.all(|(left, right)| self.are_types_equal(storage, *left, *right))
            }
            (MockType::Object(left), MockType::Object(right)) => {
                let mut zipped = left.iter().zip(right.iter());
                zipped.all(|((left_key, left_ty), (right_key, right_ty))| {
                    left_key == right_key && self.are_types_equal(storage, *left_ty, *right_ty)
                })
            }
            _ => left_ty == right_ty,
        }
    }

    fn is_type_assignable_to(
        &mut self,
        storage: &Self::TypeStorage,
        left: Self::TypeId,
        right: Self::TypeId,
    ) -> bool {
        unimplemented!()
    }

    fn is_type_subtype_of(
        &mut self,
        storage: &Self::TypeStorage,
        left: Self::TypeId,
        right: Self::TypeId,
    ) -> bool {
        unimplemented!()
    }

    fn make_union(
        &mut self,
        storage: &Self::TypeStorage,
        of: impl Iterator<Item = Self::TypeId>,
        life: TypeLife,
    ) -> Self::TypeId {
        let mut new_union_types: Vec<Self::TypeId> = vec![];

        'outer: for ty_id in of {
            let ty = storage.get_type(ty_id);
            if let MockType::Union(union) = &ty {
                // Nested loop for unions
                'outer_union: for &ty_id in &union.variants {
                    for &existing_id in new_union_types.iter() {
                        if self.are_types_equal(storage, ty_id, existing_id) {
                            continue 'outer_union;
                        }
                    }

                    new_union_types.push(ty_id);
                }
            } else {
                for &existing_id in new_union_types.iter() {
                    if self.are_types_equal(storage, ty_id, existing_id) {
                        continue 'outer;
                    }
                }
            }

            new_union_types.push(ty_id);
        }

        if new_union_types.len() == 1 {
            return new_union_types[0];
        }

        let new_union = MockUnionType::new(new_union_types);
        let new_union_id = storage.insert_type(MockType::Union(new_union), life);

        new_union_id
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum MockType {
    Union(MockUnionType),
    Object(MockObjectType),
    Other(MockOtherType),
}

impl AbstractType for MockType {
    type Metadata = ();

    fn get_metadata(&self) -> &Self::Metadata {
        match self {
            Self::Union(_) => &(),
            Self::Object(_) => &(),
            Self::Other(_) => &(),
        }
    }
}

impl IsType<MockObjectType> for MockType {
    fn as_type(&self) -> Option<&MockObjectType> {
        match self {
            Self::Object(ty) => Some(ty),
            _ => None,
        }
    }

    fn from_type(ty: MockObjectType, metadata: Self::Metadata) -> Self {
        Self::Object(ty)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct MockOtherType(&'static str);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct MockObjectType {
    // These are always sorted by key
    entries: Vec<(&'static str, MockTypeId)>,
}

impl MockObjectType {
    fn new(mut entries: Vec<(&'static str, MockTypeId)>) -> Self {
        entries.sort_by(|(a, _), (b, _)| a.cmp(b));
        Self { entries }
    }
}

impl ContainerType for MockObjectType {
    type Key = &'static str;
    type Value = MockTypeId;

    fn get(&self, key: &Self::Key) -> Option<&Self::Value> {
        self.entries
            .iter()
            .find_map(|(k, v)| if k == key { Some(v) } else { None })
    }

    fn keys(&self) -> impl '_ + Iterator<Item = &Self::Key> {
        self.entries.iter().map(|(k, _)| k)
    }
    fn iter(&self) -> impl '_ + Iterator<Item = (&Self::Key, &Self::Value)> {
        self.entries.iter().map(|(k, v)| (k, v))
    }
    fn values(&self) -> impl '_ + Iterator<Item = &Self::Value> {
        self.entries.iter().map(|(_, v)| v)
    }
    fn len(&self) -> usize {
        self.entries.len()
    }

    fn new(pairs: impl Iterator<Item = (Self::Key, Self::Value)>) -> Self {
        let mut entries = pairs.collect::<Vec<_>>();
        MockObjectType::new(entries)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct MockUnionType {
    // These are always sorted
    variants: Vec<MockTypeId>,
}

impl MockUnionType {
    fn new(mut inner_types: Vec<MockTypeId>) -> Self {
        inner_types.sort();
        Self {
            variants: inner_types,
        }
    }
}

impl UnionType for MockUnionType {
    type InnerTypeIds = MockTypeId;

    fn variants(&self) -> Cow<'_, [Self::InnerTypeIds]> {
        Cow::Borrowed(&self.variants)
    }

    fn new_non_normalized(variants: Vec<Self::InnerTypeIds>) -> Self {
        Self { variants }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum MockTypeFlat {
    Union(MockUnionTypeFlat),
    Object(MockObjectTypeFlat),
    Other(MockOtherTypeFlat),
}

impl MockTypeFlat {
    fn other(inner: &'static str) -> Self {
        Self::Other(MockOtherTypeFlat(inner))
    }

    fn object(mut entries: Vec<(&'static str, MockTypeFlat)>) -> Self {
        Self::Object(MockObjectTypeFlat::new(entries))
    }

    fn union(mut inner_types: Vec<MockTypeFlat>) -> Self {
        Self::Union(MockUnionTypeFlat::new(inner_types))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct MockOtherTypeFlat(&'static str);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct MockObjectTypeFlat {
    // These are always sorted by key
    entries: Vec<(&'static str, MockTypeFlat)>,
}

impl MockObjectTypeFlat {
    fn new(mut entries: Vec<(&'static str, MockTypeFlat)>) -> Self {
        entries.sort_by(|(a, _), (b, _)| a.cmp(b));
        Self { entries }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct MockUnionTypeFlat {
    // These are always sorted
    variants: Vec<MockTypeFlat>,
}

impl MockUnionTypeFlat {
    fn new(inner_types: Vec<MockTypeFlat>) -> Self {
        Self {
            variants: inner_types,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_union_shrink() {
        let mut storage = MockTypeStorage::new();
        let mut validation = MockTypeValidation::new();

        #[rustfmt::skip]
        let flat_ty = MockUnionTypeFlat::new(vec![
            MockTypeFlat::object(vec![
                ("a",MockTypeFlat::other("1")),
                ("b",MockTypeFlat::other("1")),
            ]),
            MockTypeFlat::object(vec![
                ("a",MockTypeFlat::other("2")),
                ("b",MockTypeFlat::other("1")),
            ]),
            MockTypeFlat::object(vec![
                ("a",MockTypeFlat::other("1")),
                ("b",MockTypeFlat::other("2")),
            ]),
            MockTypeFlat::object(vec![
                ("a",MockTypeFlat::other("2")),
                ("b",MockTypeFlat::other("2")),
            ]),
        ]);

        let union = storage.from_flat_union(flat_ty);
        let shrunk = UnionShrink::<MockObjectType>::shrink(&mut storage, &mut validation, &union);
        let shunk_flat = storage.to_flat_union(shrunk);

        let expected = MockUnionTypeFlat::new(vec![MockTypeFlat::object(vec![
            (
                "a",
                MockTypeFlat::union(vec![MockTypeFlat::other("1"), MockTypeFlat::other("2")]),
            ),
            (
                "b",
                MockTypeFlat::union(vec![MockTypeFlat::other("1"), MockTypeFlat::other("2")]),
            ),
        ])]);

        assert_eq!(shunk_flat, expected);
    }

    #[test]
    fn test_union_shrink_2() {
        let mut storage = MockTypeStorage::new();
        let mut validation = MockTypeValidation::new();

        #[rustfmt::skip]
        let flat_ty = MockUnionTypeFlat::new(vec![
            MockTypeFlat::object(vec![
                ("a",MockTypeFlat::other("1")),
                ("b",MockTypeFlat::other("1")),
            ]),
            MockTypeFlat::object(vec![
                ("a",MockTypeFlat::other("2")),
                ("b",MockTypeFlat::other("1")),
            ]),
            MockTypeFlat::object(vec![
                ("a",MockTypeFlat::other("3")),
                ("b",MockTypeFlat::other("1")),
            ]),
            MockTypeFlat::object(vec![
                ("a",MockTypeFlat::other("1")),
                ("b",MockTypeFlat::other("2")),
            ]),
            MockTypeFlat::object(vec![
                ("a",MockTypeFlat::other("2")),
                ("b",MockTypeFlat::other("2")),
            ]),
            MockTypeFlat::object(vec![
                ("a",MockTypeFlat::other("3")),
                ("b",MockTypeFlat::other("2")),
            ]),
            MockTypeFlat::object(vec![
                ("a",MockTypeFlat::other("1")),
                ("b",MockTypeFlat::other("3")),
            ]),
            MockTypeFlat::object(vec![
                ("a",MockTypeFlat::other("2")),
                ("b",MockTypeFlat::other("3")),
            ]),
            MockTypeFlat::object(vec![
                ("a",MockTypeFlat::other("3")),
                ("b",MockTypeFlat::other("3")),
            ]),
        ]);

        let union = storage.from_flat_union(flat_ty);
        let shrunk = UnionShrink::<MockObjectType>::shrink(&mut storage, &mut validation, &union);
        let shunk_flat = storage.to_flat_union(shrunk);

        let expected = MockUnionTypeFlat::new(vec![MockTypeFlat::object(vec![
            (
                "a",
                MockTypeFlat::union(vec![
                    MockTypeFlat::other("1"),
                    MockTypeFlat::other("2"),
                    MockTypeFlat::other("3"),
                ]),
            ),
            (
                "b",
                MockTypeFlat::union(vec![
                    MockTypeFlat::other("1"),
                    MockTypeFlat::other("2"),
                    MockTypeFlat::other("3"),
                ]),
            ),
        ])]);

        assert_eq!(shunk_flat, expected);
    }

    #[test]
    fn test_union_shrink_invalid() {
        let mut storage = MockTypeStorage::new();
        let mut validation = MockTypeValidation::new();

        let flat_ty = MockUnionTypeFlat::new(vec![
            MockTypeFlat::object(vec![
                (
                    "a",
                    MockTypeFlat::union(vec![MockTypeFlat::other("1"), MockTypeFlat::other("2")]),
                ),
                ("b", MockTypeFlat::other("1")),
            ]),
            MockTypeFlat::object(vec![
                ("a", MockTypeFlat::other("2")),
                (
                    "b",
                    MockTypeFlat::union(vec![MockTypeFlat::other("1"), MockTypeFlat::other("2")]),
                ),
            ]),
        ]);

        let union = storage.from_flat_union(flat_ty.clone());
        let shrunk = UnionShrink::<MockObjectType>::shrink(&mut storage, &mut validation, &union);
        let shunk_flat = storage.to_flat_union(shrunk);

        assert_eq!(shunk_flat, flat_ty);
    }
}
