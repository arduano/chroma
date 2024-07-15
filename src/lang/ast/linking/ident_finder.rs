use std::{collections::HashMap, sync::Arc};

use super::VariableId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LinkingIdentKind {
    Variable(VariableId),
}

pub struct LinkingIdentFinder<'a> {
    // OPT: Many of these will have 1 parent, maybe use an enum for those use cases?
    parents: Vec<&'a LinkingIdentFinder<'a>>,
    local_idents: HashMap<Arc<str>, LinkingIdentKind>,
}

impl<'a> LinkingIdentFinder<'a> {
    pub fn new() -> Self {
        Self {
            parents: Vec::new(),
            local_idents: HashMap::new(),
        }
    }

    pub fn new_with_parent(parent: &'a LinkingIdentFinder<'a>) -> Self {
        Self {
            parents: vec![parent],
            local_idents: HashMap::new(),
        }
    }

    pub fn new_with_parents(parents: Vec<&'a LinkingIdentFinder<'a>>) -> Self {
        Self {
            parents,
            local_idents: HashMap::new(),
        }
    }

    pub fn add_local_ident(&mut self, ident: Arc<str>, kind: LinkingIdentKind) {
        self.local_idents.insert(ident, kind);
    }

    pub fn find_ident(&self, ident: &Arc<str>) -> Option<LinkingIdentKind> {
        if let Some(&kind) = self.local_idents.get(ident) {
            return Some(kind);
        }

        for parent in self.parents.iter() {
            if let Some(kind) = parent.find_ident(ident) {
                return Some(kind);
            }
        }

        None
    }
}
