use std::fmt::{Display, Formatter};
use regex::Regex;
use crate::lr_ast;
use crate::lr_ast::Terminal::{Empty, Identifier, StringLiteral};

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Decl {
    pub(crate) identifier: String,
    pub(crate) maps_to: NonTerminal
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum BinaryOperation {
    Or,
    Concat
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum UnaryOperation {
    Plus,
    Star,
    Optional
}

#[derive(Debug, Clone, PartialEq)]
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

impl PartialEq for Terminal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (StringLiteral(r), StringLiteral(r2)) => r.to_string().eq(&r2.to_string()),
            (Identifier(s), Identifier(s2)) => s.eq(s2),
            (Empty, Empty) => true,
            (_, _) => false
        }
    }
}

impl Eq for Terminal {}


#[cfg(test)]
mod test {
    use regex::Regex;
    use crate::lr_ast::Terminal::{Empty, Identifier};

    #[test]
    fn test_eq_e_2_e() {
        assert_eq!(Empty, Empty)
    }

    #[test]
    fn test_eq_cat_2_cat() {
        assert_eq!(Identifier("cat".to_string()), Identifier("cat".to_string()))
    }

    #[test]
    fn test_eq_cat_2_cot() {
        assert_ne!(Identifier("cat".to_string()), Identifier("cot".to_string()))
    }
}