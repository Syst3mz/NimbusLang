use std::fmt::{Display, Formatter};
use regex::Regex;

#[derive(Debug, Clone)]
pub(crate) struct Decl {
    pub(crate) identifier: String,
    pub(crate) maps_to: NonTerminal
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum BinaryOperation {
    Or,
    Concat
}

#[derive(Debug, Clone)]
pub(crate) enum UnaryOperation {
    Plus,
    Star,
    Optional
}

#[derive(Debug, Clone)]
pub(crate) enum NonTerminal {
    Binary {
        lhs: Box<NonTerminal>,
        rhs: Box<NonTerminal>,
        bop: BinaryOperation
    },

    Unary {
        uop: UnaryOperation,
        lhs: Box<NonTerminal>
    },
    Term(Terminal)
}

#[derive(Debug, Clone)]
pub(crate) enum Terminal {
    Identifier(String),
    StringLiteral(Regex),
    Empty
}