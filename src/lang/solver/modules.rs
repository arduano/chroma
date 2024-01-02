use std::{
    collections::{BTreeMap, VecDeque},
    sync::Arc,
};

use crate::lang::{
    entity_ids::{Id, KnownItemHandler},
    tokens::TkIdent,
};

use super::{DcTypeDefineAbstract, TyType};

pub enum DcModuleItem {
    TypeDefine(DcTypeDefineAbstract),
}

pub struct DcModule {
    symbols: ModuleScopeIdentMatcher,
}

impl DcModule {
    pub fn get_matcher(&self) -> &ModuleScopeIdentMatcher {
        &self.symbols
    }

    pub fn from_symbol_map(
        idents: BTreeMap<Arc<str>, ModuleScopeIdent>,
        imports: Vec<Id<DcModule>>,
        modules: KnownItemHandler<DcModule>,
    ) -> Self {
        Self {
            symbols: ModuleScopeIdentMatcher {
                idents,
                imports,
                modules,
            },
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ModuleScopeIdent {
    Type(Id<TyType>),
}

struct ModuleScopeIdentMatcherInner {}

pub struct ModuleScopeIdentMatcher {
    idents: BTreeMap<Arc<str>, ModuleScopeIdent>,
    imports: Vec<Id<DcModule>>,
    modules: KnownItemHandler<DcModule>,
}

impl ModuleScopeIdentMatcher {
    pub async fn find(&self, ident: &TkIdent) -> Option<ModuleScopeIdent> {
        // First, check if the current module contains the ident
        if let Some(val) = self.idents.get(&ident.ident).map(|i| i.clone()) {
            return Some(val);
        }

        // Prevent recursion
        let mut explored_modules = Vec::new();

        // Then, perform BFS along all other imported modules to find the ident
        let mut stack = VecDeque::new();
        for &id in self.imports.iter() {
            stack.push_back(id);
        }

        while let Some(id) = stack.pop_front() {
            if explored_modules.contains(&id) {
                continue;
            }
            explored_modules.push(id);

            let module = self.modules.get(id).await;
            if let Some(val) = module.symbols.idents.get(&ident.ident).map(|i| i.clone()) {
                return Some(val);
            }

            for &id in module.symbols.imports.iter() {
                stack.push_back(id);
            }
        }

        None
    }
}
