use std::iter::Peekable;
use std::slice::Iter;
use crate::counter::{Counted, Counter};
use crate::nimbus_ast::{Ast, AstDecl};
use crate::nimbus_lexer::{Location, Token, TokenType};
use crate::nimbus_lexer::TokenType::{Comma, Fn};
use crate::nimbus_parser::ParserErrorType::{EOI, UnexpectedToken};

#[derive(Debug)]
enum ParserErrorType {
    EOI,
    UnexpectedToken
}

#[derive(Debug)]
struct ParserError {
    error_type: ParserErrorType,
    at: Location,
    message: String
}

impl ParserError {
    fn eoi(l: &Location) -> ParserError {
        ParserError {
            error_type: EOI,
            at: l.clone(),
            message: "Reached end of stream while parsing.".to_string(),
        }
    }

    fn unexpected_token(found: &Token, expected: Vec<TokenType>) -> Self {

        let options = if expected.len() == 1 {
            format!("Expected {}", expected[0])
        }
        else {
            let mut folded = String::from("Expected one of ");
            for x in expected {
                folded.push_str(&format!("{}, ", x))
            }
            folded[0..folded.len()-2].to_string().push_str(", but got")
        };

        ParserError {
            error_type: UnexpectedToken,
            at: found.location.clone(),
            message: format!("Unexpected token in stream. {} {}", options, found.tok_type),
        }
    }
}

struct Parser {
    tokens: Vec<Token>,
    counter: Counter,
    idx: usize
}

impl Parser {
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
        match self.next()? {
            &Fn => self.parse_fn(),
            &tok => Err(ParserError {
                error_type: UnexpectedToken,
                at: tok.location,
                message: "".to_string(),
            })
        }
    }

    fn tmatch(&self, t:TokenType) -> Option<&Token> {
        if self.tokens[self.idx].tok_type == t{
            Some(tok)
        }
        None
    }

    fn not_tmatch(&self, t:TokenType) -> Option<&Token> {
        if elf.tokens[self.idx].tok_type == t{
            None
        }
        Some(tok)
    }

    fn expect(&self, t:TokenType) -> bool {
        self.tokens[self.idx].tok_type == t
    }

    fn next(&mut self) -> Result<&Token, ParserError> {
        self.idx += 1;
        match self.tokens.get(self.idx) {
            None => Err(ParserError::eoi(&self.tokens[self.idx-1].location)),
            Some(t) => Ok(t)
        }
    }

    fn peek(&self, by:usize) -> Result<&Token, ParserError> {
        match self.tokens.get(self.idx + by) {
            None => Err(ParserError::eoi(&self.tokens[self.idx-1].location)),
            Some(t) => Ok(t)
        }
    }

    fn last(&mut self) {
        self.idx -= 1;
        self.idx -= usize::max(self.idx, 0);
    }

    fn at_eoi(&self) -> bool {
        self.idx < self.tokens.len()
    }
    fn parse_fn(&mut self) -> Result<Counted<AstDecl>, ParserError> {

    }

    fn parse_list<T, F:FnMut(&mut self, &Token) -> T>(&mut self, start: TokenType, end: TokenType, apply: F) -> Result<Vec<T>, ParserError> {

        // Check that the "list" starts with the start char
        if let Some(errTok) = self.not_tmatch(start) {
            Err(ParserError::unexpected_token(errTok, vec![start]))
        }

        let mut ret:Vec<T> = Vec::new();
        while !self.at_eoi() {
            ret.push(apply(self, &self.tokens[self.idx]));


            // match against the next token, and if it is not a comma, then throw an err. Otherwise, consume the comma and move on
            if Some(errTok) = self.not_tmatch(TokenType::Comma) {
                Err(ParserError::unexpected_token(errTok, vec![Comma]))
            }
            self.next()
        }
        if let Some(errTok) = self.not_tmatch(end) {
            Err(ParserError::unexpected_token(errTok, vec![end]))
        }

        Ok(ret)
    }
}

impl From<Vec<Token>> for Parser {
    fn from(vec: Vec<Token>) -> Self {
        Self {
            tokens: vec,
            counter: Counter::new(),
            idx: 0
        }
    }
}