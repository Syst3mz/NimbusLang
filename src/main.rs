use std::path::PathBuf;
use std::fs;
use crate::lr_generator::Language;
use crate::lr_generator::NonTerminal::Application;
use crate::lr_generator::Rule::{NonTerminal, Terminal};

mod nimbus_lexer;
mod nimbus_parser;
mod nimbus_ast;
mod either;
mod counter;
mod lr_generator;


fn main() {
    /*
    let path = PathBuf::from("fibo.nbs");
    let tokens = nimbus_lexer::NimbusLexer::new(
        &fs::read_to_string(path)
            .expect("unable to read file"))
        .to_vec();
    for token in tokens {
        println!("{:?}", token)
    }
    */

    let lang = Language { lang: vec![] };

    println!("{}", x.to_string())
}
