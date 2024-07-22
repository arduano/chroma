use std::{collections::HashMap, sync::Arc};

use super::VariableId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LinkingIdentKind {
    Variable(VariableId),
    NotFound,
}

pub struct LinkingIdentFinder<'a> {
    inherited: Vec<&'a LinkingIdentFinder<'a>>,
    local_parent_scopes: Vec<HashMap<Arc<str>, LinkingIdentKind>>,
    current_scope: HashMap<Arc<str>, LinkingIdentKind>,
}

impl<'a> LinkingIdentFinder<'a> {
    pub fn new() -> Self {
        Self {
            inherited: Vec::new(),
            local_parent_scopes: Vec::new(),
            current_scope: HashMap::new(),
        }
    }

    pub fn new_with_inherited(inherited: Vec<&'a LinkingIdentFinder<'a>>) -> Self {
        Self {
            inherited,
            local_parent_scopes: Vec::new(),
            current_scope: HashMap::new(),
        }
    }

    pub fn add_local_ident(&mut self, ident: Arc<str>, kind: LinkingIdentKind) {
        self.current_scope.insert(ident, kind);
    }

    pub fn find_ident(&self, ident: &Arc<str>) -> LinkingIdentKind {
        if let Some(&kind) = self.current_scope.get(ident) {
            return kind;
        }

        for parent in self.local_parent_scopes.iter().rev() {
            if let Some(&kind) = parent.get(ident) {
                return kind;
            }
        }

        for inherited in self.inherited.iter() {
            let found = inherited.find_ident(ident);
            if found != LinkingIdentKind::NotFound {
                return found;
            }
        }

        LinkingIdentKind::NotFound
    }

    pub fn push_child_scope(&mut self) {
        let parent_scope = std::mem::replace(&mut self.current_scope, HashMap::new());
        self.local_parent_scopes.push(parent_scope);
    }

    pub fn pop_child_scope(&mut self) {
        self.current_scope = self.local_parent_scopes.pop().unwrap();
    }
}
