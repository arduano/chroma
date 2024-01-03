use std::{
    collections::{BTreeMap, VecDeque},
    sync::Arc,
};

use crate::lang::{
    entity_ids::{Id, KnownItemHandler},
    tokens::TkIdent,
};

use super::{DcModule, ModuleScopeDecl};

