#![allow(dead_code)]

use std::{collections::HashMap, ops::Deref, path::PathBuf, str::FromStr, sync::Arc};

use chroma::lang::analysis::{AnBlockLinks, AnVariableDataSources};
use chroma::lang::ast::items::{SyBodyStatement, SyDeclaration, SyDeclarationBodyItem};
use chroma::lang::ast::linking::{parse_function_ast, FunctionLinkingCompilation};
use chroma::lang::tokens::parse_file;

use chroma::lang::{
    ast::{
        helpers::{AstItem, AstParser, ParsingPhaseEnv},
        items::SyDeclarationBody,
    },
    solver_old::{CodeFilePath, IdCounter, KnownFiles, ModuleGroupCompilation},
    tokens::TokenReader,
    ErrorCollector,
};

// Naming:
// Tk__ - Tokens
// Sy__ - AST
// Li__ - Linked and simplified definitions, resolving any AST names to IDs
// An__ - Analysis
// Ty__ - Raw type system
// Ts__ - Type scope, for type functions and the global scope
// Rt__ - Runtime Data, execution logic

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
    let mut known_files = KnownFiles::new();

    let test_file_path = PathBuf::from_str("./test.cm").unwrap();
    let text: Arc<str> = std::fs::read_to_string(&test_file_path).unwrap().into();

    let file_id = known_files.add_file(CodeFilePath::from_path(test_file_path), text.clone());
    let file_ref = known_files.get_ref_for_file_id(file_id);

    let tokens = parse_file(file_ref, &text);

    // let mut module_counter = IdCounter::new();

    // dbg!(&tokens);

    let mut error_collector = ErrorCollector::new();

    let mut ast_parser = AstParser::new(TokenReader::new(&tokens.tokens), error_collector.clone());

    let env = ParsingPhaseEnv::new();
    let ast = SyDeclarationBody::parse(&mut ast_parser, env).unwrap();

    dbg!(&ast);

    let function = &ast.statements.first().unwrap().as_ref().unwrap().item;
    let SyDeclaration::Function(function) = function else {
        panic!("Expected function");
    };

    let mut function_linking_compilation = FunctionLinkingCompilation {
        errors: &mut error_collector,
    };

    let linked_function = parse_function_ast(function, vec![], &mut function_linking_compilation);

    println!("{}", linked_function);

    let block_links = AnBlockLinks::analyze_block_links_for(&linked_function);
    dbg!(&block_links);

    let variable_data_sources =
        AnVariableDataSources::analyze_variable_data_sources_for(&linked_function, &block_links);
    dbg!(&variable_data_sources);

    // let mut compilation = ModuleGroupCompilation::new(module_counter.next(), HashMap::new());
    // let _type_ids = compilation.compile_in_ast(None, &ast);

    // let a_li_type_id = compilation.linked_type_definitions.keys().find(|k| {
    //     let Some(name) = &compilation.linked_type_definitions[k].name else {
    //         return false;
    //     };

    //     name.ident.deref() == "A"
    // });
    // let a_type_id = *compilation
    //     .linked_type_to_type_mapping
    //     .get(&a_li_type_id.unwrap())
    //     .unwrap();

    // let b_li_type_id = compilation.linked_type_definitions.keys().find(|k| {
    //     let Some(name) = &compilation.linked_type_definitions[k].name else {
    //         return false;
    //     };

    //     name.ident.deref() == "B"
    // });
    // let b_type_id = *compilation
    //     .linked_type_to_type_mapping
    //     .get(&b_li_type_id.unwrap())
    //     .unwrap();

    // dbg!(&compilation.linked_type_definitions);
    // dbg!(&compilation.linked_type_to_type_mapping);
    // dbg!(&compilation.type_data.types);

    // let assignable = run_type_assignability_query(
    //     &compilation.type_data.types,
    //     &mut compilation.type_data.type_assignability,
    //     a_type_id,
    //     b_type_id,
    // );
    // dbg!(assignable);

    let mut errors = Vec::new();
    errors.extend_from_slice(&tokens.errors);
    errors.extend_from_slice(&error_collector.errors());
    // errors.extend_from_slice(&compilation.errors.errors());

    dbg!(&errors);
}
