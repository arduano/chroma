use std::{collections::BTreeMap, path::PathBuf, str::FromStr, sync::Arc};

use lang::{
    entity_ids::{Id, KnownItemHandler},
    modules,
    solver::{DcModule, ModuleScopeIdent, TyString, TyType, TyTypeKind},
    tokens::parse_file,
};

use crate::lang::{
    ast::{
        helpers::{AstItem, AstParser, ParsingPhaseEnv},
        items::{SyDeclarationBody, SyModule},
    },
    tokens::{FileRef, TokenReader},
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

    let symbol_map = BTreeMap::from([(Arc::from("string"), ModuleScopeIdent::Type(string_id))]);

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

    let mut ast_parser = AstParser::new(TokenReader::new(&tokens.tokens));

    let env = ParsingPhaseEnv::new();
    let ast = SyDeclarationBody::parse(&mut ast_parser, env);

    dbg!(ast);
    dbg!(ast_parser.errors());
}
