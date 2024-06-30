use std::{collections::HashMap, sync::Arc};

use crate::lang::tokens::TkIdent;

use super::{
    linked_ast::{LiExpression, LiTypeFnArg, LiTypeFnLazyValue},
    Id, MId,
};

pub struct ModuleNamespace {
    items: HashMap<Arc<str>, ModuleNamespaceItem>,
}

impl ModuleNamespace {
    pub fn new() -> Self {
        Self {
            items: HashMap::new(),
        }
    }

    pub fn add_item(&mut self, item: ModuleNamespaceItem) {
        let existing = self.items.insert(item.ident.ident.clone(), item);
        assert!(existing.is_none());
    }

    pub fn get_ident_kind(&self, ident: &TkIdent) -> Option<ModuleNamespaceItemKind> {
        Some(self.items.get(&ident.ident)?.kind)
    }

    pub fn get_item(&self, ident: &Arc<str>) -> Option<ModuleNamespaceItem> {
        self.items.get(ident).cloned()
    }

    pub fn items(&self) -> impl '_ + Iterator<Item = ModuleNamespaceItem> {
        self.items.values().cloned()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleNamespaceItem {
    pub ident: TkIdent,
    pub kind: ModuleNamespaceItemKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ModuleNamespaceItemKind {
    Type(MId<LiExpression>),
    Unknown,
}

pub struct FunctionNamespace<'a> {
    module: &'a ModuleNamespace,
    items: HashMap<Arc<str>, FunctionNamespaceItem>,
}

impl<'a> FunctionNamespace<'a> {
    pub fn new(module: &'a ModuleNamespace) -> Self {
        Self {
            module,
            items: HashMap::new(),
        }
    }

    pub fn add_item(&mut self, item: FunctionNamespaceItem) {
        let existing = self.items.insert(item.ident.ident.clone(), item);
        assert!(existing.is_none());
    }

    pub fn get_ident_kind(&self, ident: &TkIdent) -> Option<FunctionNamespaceItemKind> {
        let item = self.get_item(&ident.ident)?;
        Some(item.kind)
    }

    pub fn get_item(&self, ident: &Arc<str>) -> Option<FunctionNamespaceItem> {
        if let Some(item) = self.items.get(ident) {
            return Some(item.clone());
        }

        let module_item = self.module.get_item(ident)?;
        let function_item = FunctionNamespaceItem {
            ident: module_item.ident.clone(),
            kind: FunctionNamespaceItemKind::ModuleItem(module_item.kind),
        };
        Some(function_item)
    }

    pub fn local_items(&self) -> impl '_ + Iterator<Item = FunctionNamespaceItem> {
        self.items.values().cloned()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionNamespaceItem {
    pub ident: TkIdent,
    pub kind: FunctionNamespaceItemKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FunctionNamespaceItemKind {
    ModuleItem(ModuleNamespaceItemKind),
    TypeFnLazyType(Id<LiTypeFnLazyValue>),
    TypeFnArg(Id<LiTypeFnArg>),
    Unknown,
}
