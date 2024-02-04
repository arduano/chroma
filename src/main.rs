#![allow(dead_code)]

use std::{path::PathBuf, str::FromStr};

use lang::tokens::parse_file;

use crate::lang::{
    ast::{
        helpers::{AstItem, AstParser, ParsingPhaseEnv},
        items::SyDeclarationBody,
    },
    entity_ids::IdCounter,
    solver::ModuleGroupCompilation,
    tokens::{FileRef, TokenReader},
    ErrorCollector,
};

mod lang;

// Naming:
// Tk__ - Tokens
// Sy__ - AST
// Li__ - Linked and simplified definitions, resolving any AST names to IDs
// Ty__ - Raw type system
// St__ - Static Compiler Data (e.g. types, generic function descriptions)
// Rt__ - Runtime Data (e.g. values, generic function instances)

// fn build_std_module(
//     modules: &KnownItemHandler<DcModule>,
//     types: &KnownItemHandler<TyType>,
// ) -> Id<DcModule> {
//     let string_id = types.allocate_value(TyType::new(TyTypeKind::String(TyString::new())));

//     let string_decl = DcTypeDefine {
//         ast: None,
//         type_id: string_id,
//     };

//     let symbol_map = BTreeMap::from([(
//         Arc::from("string"),
//         Arc::new(ModuleScopeDecl::TypeDecl(string_decl)),
//     )]);

//     let mod_id = modules.allocate_value(DcModule::from_symbol_map(
//         symbol_map,
//         vec![],
//         modules.clone(),
//     ));

//     mod_id
// }

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
    let ast = SyDeclarationBody::parse(&mut ast_parser, env).unwrap();

    let mut module_counter = IdCounter::new();

    let mut compilation = ModuleGroupCompilation::new_without_deps(module_counter.next());
    let _type_ids = compilation.compile_in_ast(None, &ast);

    // let modules = KnownItemHandler::new();
    // let types = KnownItemHandler::new();

    // let std_mod_id = build_std_module(&modules, &types);

    dbg!(&ast);

    // let types: Vec<_> = type_ids
    //     .iter()
    //     .map(|id| (id, &compilation.types[*id]))
    //     .collect();

    // dbg!(types);
    dbg!(compilation.linked_type_definitions);
    dbg!(compilation.linked_type_to_type_mapping);
    dbg!(compilation.types);

    dbg!(&errors.errors());
    dbg!(&compilation.errors.errors());
}
