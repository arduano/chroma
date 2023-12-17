use std::{path::PathBuf, str::FromStr};

use syntax::tokens::parse_file;

use crate::syntax::{
    ast::{AstItem, AstParser, ParsingPhaseEnv, STypeFn},
    tokens::{FileRef, TokenReader},
};

mod syntax;

fn main() {
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
    let ast = STypeFn::parse(&mut ast_parser, env);

    dbg!(ast);
    dbg!(ast_parser.errors());
}
