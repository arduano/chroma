use std::{
    collections::{BTreeMap, VecDeque},
    sync::Arc,
};

use crate::lang::{
    ast::items::SyDeclarationBody,
    entity_ids::{Id, KnownItemHandler},
    tokens::TkIdent,
};

use super::DcTypeDefine;

pub enum DcModuleItem {
    TypeDefine(DcTypeDefine),
}

pub struct DcModule {
    ast: Option<Arc<SyDeclarationBody>>,
    symbols: ModuleScopeIdentMatcher,
}

impl DcModule {
    pub fn get_matcher(&self) -> &ModuleScopeIdentMatcher {
        &self.symbols
    }

    pub fn from_symbol_map(
        idents: BTreeMap<Arc<str>, Arc<ModuleScopeDecl>>,
        imports: Vec<Id<DcModule>>,
        modules: KnownItemHandler<DcModule>,
    ) -> Self {
        Self {
            ast: None,
            symbols: ModuleScopeIdentMatcher {
                idents,
                imports,
                modules,
            },
        }
    }

    pub fn new_from_ast_and_symbol_map(
        ast: Arc<SyDeclarationBody>,
        idents: BTreeMap<Arc<str>, Arc<ModuleScopeDecl>>,
        imports: Vec<Id<DcModule>>,
        modules: KnownItemHandler<DcModule>,
    ) -> Self {
        Self {
            ast: Some(ast),
            symbols: ModuleScopeIdentMatcher {
                idents,
                imports,
                modules,
            },
        }
    }
}

#[derive(Debug)]
pub enum ModuleScopeDecl {
    TypeDecl(DcTypeDefine),
}

pub struct ModuleScopeIdentMatcher {
    idents: BTreeMap<Arc<str>, Arc<ModuleScopeDecl>>,
    imports: Vec<Id<DcModule>>,
    modules: KnownItemHandler<DcModule>,
}

impl ModuleScopeIdentMatcher {
    pub async fn find(&self, ident: &TkIdent) -> Option<Arc<ModuleScopeDecl>> {
        // First, check if the current module contains the ident
        if let Some(val) = self.idents.get(&ident.ident).map(|i| i.clone()) {
            return Some(val.clone());
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
                return Some(val.clone());
            }

            for &id in module.symbols.imports.iter() {
                stack.push_back(id);
            }
        }

        None
    }
}

pub enum RuntimeParentIdentMatcher {
    RuntimeScope(Arc<RuntimeScopeIdentMatcher>),
    ModuleScope(Id<DcModule>),
}

#[derive(Debug)]
pub enum RuntimeScopeDecl {
    ModuleDecl(Arc<ModuleScopeDecl>),
}

pub struct RuntimeScopeIdentMatcher {
    idents: BTreeMap<Arc<str>, Arc<RuntimeScopeDecl>>,
    imports: Vec<IdentMatcherKind>,
    modules: KnownItemHandler<DcModule>,
}

impl RuntimeScopeIdentMatcher {
    pub async fn find(&self, ident: &TkIdent) -> Option<Arc<RuntimeScopeDecl>> {
        // First, check if the current module contains the ident
        if let Some(val) = self.idents.get(&ident.ident).map(|i| i.clone()) {
            return Some(val.clone());
        }

        // Prevent recursion
        let mut explored_modules = Vec::new();

        // Then, perform BFS along all other imported modules to find the ident
        let mut stack = VecDeque::new();
        for matcher in self.imports.iter() {
            stack.push_back(matcher.clone());
        }

        while let Some(matcher) = stack.pop_front() {
            match matcher {
                IdentMatcherKind::RuntimeScope(matcher) => {
                    if let Some(val) = matcher.idents.get(&ident.ident) {
                        return Some(val.clone());
                    }

                    for matcher in matcher.imports.iter() {
                        stack.push_back(matcher.clone());
                    }
                }
                IdentMatcherKind::ModuleScope(id) => {
                    if explored_modules.contains(&id) {
                        continue;
                    }
                    explored_modules.push(id);

                    let module = self.modules.get(id).await;
                    if let Some(val) = module.symbols.idents.get(&ident.ident).map(|i| i.clone()) {
                        return Some(Arc::new(RuntimeScopeDecl::ModuleDecl(val.clone())));
                    }

                    for &id in module.symbols.imports.iter() {
                        stack.push_back(IdentMatcherKind::ModuleScope(id));
                    }
                }
            }
        }

        None
    }
}

#[derive(Clone)]
pub enum IdentMatcherKind {
    RuntimeScope(Arc<RuntimeScopeIdentMatcher>),
    ModuleScope(Id<DcModule>),
}

#[derive(Clone)]
pub struct IdentMatcher {
    kind: IdentMatcherKind,
    modules: KnownItemHandler<DcModule>,
}

impl IdentMatcher {
    pub async fn find(&self, ident: &TkIdent) -> Option<Arc<RuntimeScopeDecl>> {
        match &self.kind {
            IdentMatcherKind::RuntimeScope(matcher) => matcher.find(ident).await,
            IdentMatcherKind::ModuleScope(id) => {
                let module = self.modules.get(*id).await;
                module
                    .symbols
                    .find(ident)
                    .await
                    .map(|i| Arc::new(RuntimeScopeDecl::ModuleDecl(i)))
            }
        }
    }

    pub fn new_module_scope(id: Id<DcModule>, modules: KnownItemHandler<DcModule>) -> Self {
        Self {
            kind: IdentMatcherKind::ModuleScope(id),
            modules,
        }
    }

    pub fn new_runtime_scope(
        matcher: Arc<RuntimeScopeIdentMatcher>,
        modules: KnownItemHandler<DcModule>,
    ) -> Self {
        Self {
            kind: IdentMatcherKind::RuntimeScope(matcher),
            modules,
        }
    }
}
