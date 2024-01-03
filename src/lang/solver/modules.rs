use std::{collections::BTreeMap, sync::Arc};

use crate::lang::{
    ast::items::{SyDeclaration, SyDeclarationBody, SyTypeDefine},
    entity_ids::{Id, KnownItemHandler},
    solver::{analyze_type_expression, IdentMatcher},
    ErrorCollector,
};

use super::{DcModule, ModuleScopeDecl, TyType};

#[derive(Debug)]
pub struct DcTypeDefine {
    pub ast: Option<Arc<SyTypeDefine>>,
    pub type_id: Id<TyType>,
}

pub async fn analyze_module(
    ast: Arc<SyDeclarationBody>,
    modules: KnownItemHandler<DcModule>,
    types: KnownItemHandler<TyType>,
    error_collector: ErrorCollector,
    auto_imported_modules: Vec<Id<DcModule>>,
) -> Id<DcModule> {
    modules.clone().allocate_and_fill_with(|mod_id| async move {
        let mut idents = BTreeMap::new();
        let imports = auto_imported_modules;

        let ident_matcher = IdentMatcher::new_module_scope(mod_id, modules.clone());

        for item in ast.statements.iter() {
            let Ok(item) = item else {
                continue;
            };

            match &item.item {
                SyDeclaration::TypeDefine(ty_def) => {
                    let type_id = analyze_type_expression(
                        Some(ty_def.name.clone()),
                        ty_def.value.clone(),
                        ident_matcher.clone(),
                        types.clone(),
                        error_collector.clone(),
                    );

                    let def = DcTypeDefine {
                        ast: Some(ty_def.clone()),
                        type_id,
                    };

                    idents.insert(
                        ty_def.name.ident.clone(),
                        Arc::new(ModuleScopeDecl::TypeDecl(def)),
                    );
                }
                SyDeclaration::TypeFn(_) => todo!(),
            }
        }

        DcModule::new_from_ast_and_symbol_map(ast.clone(), idents, imports, modules.clone())
    })
}
