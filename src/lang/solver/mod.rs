use std::fmt::{Debug, Formatter};
use std::future::Future;
use std::path::PathBuf;
use std::{collections::BTreeMap, sync::Arc};

use self::type_system::TyType;

use super::ast::items::*;
use super::Shared;

mod type_system;

struct DcTypeDefine {
    name: String,
    type_: Shared<TyType>,
}

struct DcTypeDefineAbstract {
    ast: SyTypeDefine,
    resolved: DcTypeDefine,
}

enum DcModuleItem {
    TypeDefine(DcTypeDefineAbstract),
}

struct DcModule {
    symbols: BTreeMap<String, Arc<DcModuleItem>>,
}

pub struct KnownFilesMap {
    files: BTreeMap<PathBuf, Arc<SyModule>>,
}
