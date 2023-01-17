use std::iter::Peekable;
use std::slice::Iter;
use crate::counter::{Counted, Counter};
use crate::nimbus_ast::{Ast, AstDecl};
use crate::nimbus_lexer::{Location, Token};
use crate::nimbus_lexer::TokenType::Fn;
use crate::nimbus_parser::ParserError::EOI;
use crate::nimbus_parser::ParserErrorType::EOI;

#[derive(Debug)]
enum ParserErrorType {
    EOI
}

#[derive(Debug)]
struct ParserError {
    error_type: ParserErrorType,
    at: Location,
    message: String
}

impl ParserError {
    fn eoi(l: Location) -> ParserError {
        ParserError {
            error_type: EOI,
            at: l,
            message: "Reached end of stream while parsing.".to_string(),
        }
    }
}

struct Parser<'a> {
    tokens: Vec<Token>,
    counter: Counter,
    idx: usize
}

impl<'a> Parser<'a> {
    pub fn parse(&mut self) -> (Ast, Vec<ParserError>) {
        let mut ast = Ast::new();

        let mut tree: Vec<Counted<AstDecl>> = Vec::new();
        let mut errs: Vec<ParserError> = vec![];
        while !self.at_eoi() {
            tree.push(self.parse_decl().unwrap());
        }

        (ast, errs)
    }
    fn parse_decl(&mut self) -> Result<Counted<AstDecl>, ParserError> {
        if self.at_eoi() {
            Err(ParserError::eoi(self.tokens[self.idx].location))
        }

        if self.tokens[self.idx].tok_type != Fn {

        }
    }

    fn at_eoi(&self) -> bool {
        idx < self.tokens.len()
    }
}

impl From<Vec<Token>> for Parser<'_> {
    fn from(vec: Vec<Token>) -> Self {
        Self {
            tokens: vec,
            counter: Counter::new(),
            idx: 0
        }
    }
}