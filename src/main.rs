use std::path::PathBuf;
use std::fs;
use crate::nimbus_lexer::TokenType;

mod nimbus_lexer;
mod nimbus_parser;
mod nimbus_ast;
mod either;
mod counter;
mod lr_generator;


fn main() {
    let path = PathBuf::from("fibo.nbs");
    let tokens = nimbus_lexer::NimbusLexer::new(
        &fs::read_to_string(path)
            .expect("unable to read file"))
        .to_vec();
    for token in tokens {
        println!("{:?}", token)
    }

}
