use std::fmt::{Display, Formatter};
use regex::Regex;
use crate::lr_ast;
use crate::lr_ast::{BinaryOperation, Terminal};
use crate::lr_ast::Terminal::{Empty, Identifier, StringLiteral};
use crate::lr_unary_remover::NonTerminal::{Binary, Term};

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Decl {
    pub(crate) identifier: String,
    pub(crate) maps_to: NonTerminal
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum NonTerminal {
    Binary {
        lhs: Box<NonTerminal>,
        rhs: Box<NonTerminal>,
        bop: BinaryOperation
    },

    Term(Terminal)
}

pub(crate) fn remove_unary(input: Vec<lr_ast::Decl>) -> Vec<Decl> {
    let mut ret:Vec<Decl> = vec![];
    for decl in input {
       ret.push(Decl {
           identifier: decl.identifier,
           maps_to: convert(decl.maps_to),
       })
    }

    ret
}

fn convert(nt: lr_ast::NonTerminal) -> NonTerminal {
    match nt {
        lr_ast::NonTerminal::Binary { lhs, rhs, bop } => {
            Binary {
                lhs: Box::new(convert(*lhs)),
                rhs: Box::new((convert(*rhs))),
                bop,
            }
        }
        lr_ast::NonTerminal::Unary { uop, lhs } => {
            panic!("there is a unary, and there should not be")
        }
        lr_ast::NonTerminal::Term(t) => Term(t)
    }
}

#[cfg(test)]
mod test {
    use crate::lr_ast::NonTerminal::Unary;
    use crate::lr_augmenter::LrAugmenter;
    use crate::lr_unary_remover::remove_unary;

    static SELF_G: &str = r#"decl         -> IDENTIFIER "->" nonterminal
nonterminal  -> terminal
			  | nonterminal "\|" nonterminal
			  | nonterminal nonterminal
			  | nonterminal "\+"
			  | nonterminal "\*"
			  | nonterminal "\?"
			  | "\(" nonterminal "\)"
terminal     -> REGEX
			  | IDENTIFIER"#;

    #[test]
    pub fn check_no_unary() {
        let mut generator = LrAugmenter::new(SELF_G).unwrap();
        generator.augment_grammar();
        generator.augment_grammar_with_starting_production();
        let removed = remove_unary(generator.decls);
    }
}