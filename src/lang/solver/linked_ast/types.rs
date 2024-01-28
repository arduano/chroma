use crate::lang::{entity_ids::Id, solver::type_system::TyType};

pub struct LiType {
    pub type_id: Option<Id<TyType>>,
}
