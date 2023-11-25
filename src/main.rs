use std::{path::PathBuf, str::FromStr};

use syntax::tokens::parse_file;

mod syntax;

fn main() {
    let test_file_path = PathBuf::from_str("./test.cm").unwrap();
    let text = std::fs::read_to_string(&test_file_path).unwrap();

    let tokens = parse_file(&text, test_file_path.into());
    dbg!(tokens);
}
