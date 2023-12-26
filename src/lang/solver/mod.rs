use std::fmt::{Debug, Formatter};
use std::path::PathBuf;
use std::{collections::BTreeMap, sync::Arc};

use super::ast::items::*;

mod type_system;

struct Shared<T: Sync + Send> {
    value: Arc<T>,
}

impl<T: Sync + Send> Shared<T> {
    pub fn new(value: T) -> Self {
        Self {
            value: Arc::new(value),
        }
    }

    /// Gets the pointer as usize
    pub fn id(&self) -> usize {
        Arc::as_ptr(&self.value) as usize
    }
}

impl<T: Sync + Send> std::ops::Deref for Shared<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T: Debug + Sync + Send> Debug for Shared<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Shared").field(&self.value).finish()
    }
}

impl Clone for Shared<SyTypeDefine> {
    fn clone(&self) -> Self {
        Self {
            value: self.value.clone(),
        }
    }
}

enum CTypeDef {}

struct CTypeFn {}

struct LTypeDefineAbstract {
    ast: SyTypeDefine,
    type_: async_once_cell::OnceCell<CTypeDef>,
}

enum LModuleItem {
    TypeDefine(LTypeDefineAbstract),
}

struct LModule {
    symbols: BTreeMap<String, Arc<LModuleItem>>,
}

pub struct KnownFilesMap {
    files: BTreeMap<PathBuf, Arc<SyModule>>,
}
