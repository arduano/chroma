use std::{collections::BTreeMap, path::PathBuf, pin::pin, rc::Rc, str::FromStr, sync::Arc};

use futures::StreamExt;
use lang::{
    entity_ids::{Id, KnownItemHandler},
    solver_old::{DcModule, DcTypeDefine, ModuleScopeDecl, TyString, TyType, TyTypeKind},
    tokens::parse_file,
};

use crate::lang::{
    ast::{
        helpers::{AstItem, AstParser, ParsingPhaseEnv},
        items::SyDeclarationBody,
    },
    solver_old::analyze_module,
    tokens::{FileRef, TkIdent, TokenReader},
    ErrorCollector,
};

mod lang;

// Naming:
// Tk__ - Tokens
// Sy__ - AST
// Ty__ - Raw type system
// Dc__ - Declarations (e.g. functions/types declared inside a module)
// St__ - Static Compiler Data (e.g. types, generic function descriptions)
// Rt__ - Runtime Data (e.g. values, generic function instances)

fn build_std_module(
    modules: &KnownItemHandler<DcModule>,
    types: &KnownItemHandler<TyType>,
) -> Id<DcModule> {
    let string_id = types.allocate_value(TyType::new(TyTypeKind::String(TyString::new())));

    let string_decl = DcTypeDefine {
        ast: None,
        type_id: string_id,
    };

    let symbol_map = BTreeMap::from([(
        Arc::from("string"),
        Arc::new(ModuleScopeDecl::TypeDecl(string_decl)),
    )]);

    let mod_id = modules.allocate_value(DcModule::from_symbol_map(
        symbol_map,
        vec![],
        modules.clone(),
    ));

    mod_id
}

#[tokio::main]
async fn main() {
    let test_file_path = PathBuf::from_str("./test.cm").unwrap();
    let text = std::fs::read_to_string(&test_file_path).unwrap();

    let file = FileRef {
        path: test_file_path.clone(),
        contents: text.clone(),
    };

    let tokens = parse_file(file);

    dbg!(&tokens);

    let errors = ErrorCollector::new();

    let mut ast_parser = AstParser::new(TokenReader::new(&tokens.tokens), errors.clone());

    let env = ParsingPhaseEnv::new();
    let ast = SyDeclarationBody::parse(&mut ast_parser, env);

    let modules = KnownItemHandler::new();
    let types = KnownItemHandler::new();

    let std_mod_id = build_std_module(&modules, &types);

    dbg!(&ast);

    // let mod_id = analyze_module(
    //     Arc::new(ast.unwrap()),
    //     modules.clone(),
    //     types.clone(),
    //     errors.clone(),
    //     vec![std_mod_id],
    // )
    // .await;

    // let module = modules.get(mod_id).await;

    // let ident = TkIdent::new_from_str("A");
    // let a = module.get_matcher().find(&ident).await;
    // dbg!(a);

    // let mut resolved_types = BTreeMap::new();
    // let mut iter = pin!(types.iter());
    // while let Some((id, ty)) = iter.next().await {
    //     resolved_types.insert(id, ty);
    // }

    // dbg!(&resolved_types);

    dbg!(&errors.errors());
}

struct FilesList {}

fn test() {}
