use std::sync::Arc;

pub enum IdentMatcher {
    ModuleScope(ModuleScopeIdentMatcher),
    FunctionScope(FunctionScopeIdentMatcher),
}

pub struct ModuleScopeIdentMatcher {
    parent: Arc<ModuleScopeIdentMatcher>,
}

pub struct FunctionScopeIdentMatcher {
    parent: Arc<ModuleScopeIdentMatcher>,
}
