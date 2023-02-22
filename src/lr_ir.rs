use regex::Regex;
use crate::lr_ast;
use crate::lr_ir::BinaryOperation::{Concat, Or};
use crate::lr_ir::NonTerminal::{Binary, Term, Unary};
use crate::lr_ir::Terminal::{Empty, Identifier, StringLiteral};
use crate::lr_ir::UnaryOperation::{Optional, Plus, Star};

#[derive(Debug, Clone)]
pub(crate) struct Decl {
    pub(crate) identifier: String,
    pub(crate) maps_to: NonTerminal
}

#[derive(Debug, Clone)]
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
        operands: Vec<NonTerminal>,
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

pub(crate) fn linearize(input: &Vec<lr_ast::Decl>) -> Vec<Decl> {
    let mut ret:Vec<Decl> = vec![];
    for decl in input {
       ret.push(Decl {
           identifier: decl.identifier.clone(),
           maps_to: linearize_recursive(&decl.maps_to),
       })
    }

    ret
}

fn linearize_recursive(nt: &lr_ast::NonTerminal) -> NonTerminal {
    match nt {
        lr_ast::NonTerminal::Binary { lhs, rhs, bop } => {
            Binary {
                operands: linearize_bops(lhs, rhs, bop),
                bop: match bop {
                    lr_ast::BinaryOperation::Or => {Or}
                    lr_ast::BinaryOperation::Concat => {Concat}
                },
            }
        }
        lr_ast::NonTerminal::Unary { uop, lhs } => {
            Unary {
                uop: match uop {
                    lr_ast::UnaryOperation::Plus => {Plus}
                    lr_ast::UnaryOperation::Star => {Star}
                    lr_ast::UnaryOperation::Optional => {Optional}
                },
                lhs: Box::new(linearize_recursive(lhs)),
            }
        }
        lr_ast::NonTerminal::Term(term) => {Term(match term {
            lr_ast::Terminal::Identifier(id) => {Identifier(id.clone())}
            lr_ast::Terminal::StringLiteral(reg) => {StringLiteral(reg.clone())}
            lr_ast::Terminal::Empty => {Empty}
        })}
    }
}

fn linearize_bops(c_lhs: &lr_ast::NonTerminal, c_rhs: &lr_ast::NonTerminal, c_bop:&lr_ast::BinaryOperation) -> Vec<NonTerminal> {
    let mut ret: Vec<NonTerminal> = vec![];
    match c_lhs {
        lr_ast::NonTerminal::Binary { lhs, rhs, bop } => {
            if bop == c_bop {
                ret.append(&mut linearize_bops(lhs, rhs, bop))
            }
            else {
                ret.push(linearize_recursive(c_lhs))
            }
        }
        nt => ret.push(linearize_recursive(nt))
    }
    ret.push(linearize_recursive(c_rhs));
    ret
}

#[cfg(test)]
pub mod tests {
    use crate::lr_augmenter::LrAugmenter;
    use crate::lr_ir::linearize;

    #[test]
    pub fn harder_bop_collapse() {
        let g = "a -> (b c) | (d e) f+";
        let mut generator = LrAugmenter::new(g).unwrap();
        println!("simple bop: {:#?}", linearize(generator.augment_grammar()));
    }

    #[test]
    pub fn simple_bop_collapse() {
        let g = "a -> b c d";
        let mut generator = LrAugmenter::new(g).unwrap();
        println!("simple bop: {:#?}", linearize(generator.augment_grammar()));
    }
}