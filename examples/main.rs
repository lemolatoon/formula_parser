use formula_parser::formula::tokenize;
use formula_parser::formula::Parser;

use std::{
    collections::{BTreeMap, BTreeSet, HashSet},
    io,
};

fn main() {
    let mut line = String::new();
    io::stdin()
        .read_line(&mut line)
        .expect("failed to read from stdin");

    let tokens = tokenize(line);
    println!("{:?}", tokens);
    let mut parser = Parser::new(tokens);
    parser.parse();
    println!("{:?}", parser.exprs);
}
