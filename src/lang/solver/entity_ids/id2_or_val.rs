use super::Id2;

pub enum Id2OrVal<M, T> {
    Id(Id2<M, T>),
    Val(T),
}

impl<M, T> std::fmt::Debug for Id2OrVal<M, T>
where
    T: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Id(id) => f.debug_tuple("Id2").field(id).finish(),
            Self::Val(val) => f.debug_tuple("Val").field(val).finish(),
        }
    }
}

impl<M, T> Clone for Id2OrVal<M, T>
where
    T: Clone,
{
    fn clone(&self) -> Self {
        match self {
            Self::Id(id) => Self::Id(id.clone()),
            Self::Val(val) => Self::Val(val.clone()),
        }
    }
}

impl<M, T> Copy for Id2OrVal<M, T> where T: Copy {}

impl<M, T> Eq for Id2OrVal<M, T> where T: Eq {}

impl<M, T> PartialEq for Id2OrVal<M, T>
where
    T: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Id(id1), Self::Id(id2)) => id1 == id2,
            (Self::Val(val1), Self::Val(val2)) => val1 == val2,
            _ => false,
        }
    }
}

impl<M, T> std::hash::Hash for Id2OrVal<M, T>
where
    T: std::hash::Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Id(id) => id.hash(state),
            Self::Val(val) => val.hash(state),
        }
    }
}
