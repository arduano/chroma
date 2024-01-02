use std::path::PathBuf;
use std::{collections::BTreeMap, sync::Arc};

use super::ast::items::*;
use super::Shared;

mod type_system;
pub use type_system::*;

mod modules;
pub use modules::*;

struct DcTypeDefine {
    name: String,
    type_: Shared<TyType>,
}

struct DcTypeDefineAbstract {
    ast: SyTypeDefine,
    resolved: DcTypeDefine,
}

pub struct KnownFilesMap {
    files: BTreeMap<PathBuf, Arc<SyModule>>,
}
