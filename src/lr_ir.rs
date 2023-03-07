use regex::Regex;
use crate::lr_ast;
use crate::lr_ast::BinaryOperation;
use crate::lr_ir::BinaryOperation::{Concat, Or};
use crate::lr_ir::IrError::IllegalUnary;
use crate::lr_ir::NonTerminal::Term;
use crate::lr_ir::Terminal::{Empty, Identifier, StringLiteral};

#[derive(Debug, Clone)]
pub(crate) struct Decl {
    pub(crate) identifier: String,
    pub(crate) maps_to: NonTerminal
}

#[derive(Debug, Clone)]
pub(crate) enum NonTerminal {
    Concat(Vec<NonTerminal>),
    Term(Terminal)
}

#[derive(Debug, Clone)]
pub(crate) enum Terminal {
    Identifier(String),
    StringLiteral(Regex),
    Empty
}

#[derive(Debug)]
pub(crate) enum IrError {
    IllegalUnary(lr_ast::NonTerminal)
}

pub(crate) fn transform(input: &Vec<lr_ast::Decl>) -> Result<Vec<Decl>, IrError> {
    let mut ret: Vec<Decl> = vec![];
    for decl in input {
        let t = transform_decl(&mut ret, decl)?;
       ret.push(t)
    }

    Ok(ret)
}

fn transform_decl(internal_vec:&mut Vec<Decl>, decl: &lr_ast::Decl) -> Result<Decl, IrError> {
    Ok(Decl {
        identifier: decl.identifier.clone(),
        maps_to: transform_non_terminal(internal_vec, &decl.maps_to)?,
    })
}

fn transform_non_terminal(internal_vec: &mut Vec<Decl>, nt: &lr_ast::NonTerminal) -> Result<NonTerminal, IrError> {
    match nt {
        lr_ast::NonTerminal::Binary { lhs, rhs, bop } => {
            transform_binary(internal_vec, lhs.as_ref(), rhs.as_ref(), bop)
        }
        lr_ast::NonTerminal::Unary { .. } => {
            Err(IllegalUnary(nt.clone()))
        }
        lr_ast::NonTerminal::Term(t) => {
            Ok(Term(match t {
                lr_ast::Terminal::Identifier(i) => Identifier(i.clone()),
                lr_ast::Terminal::StringLiteral(s) => StringLiteral(s.clone()),
                lr_ast::Terminal::Empty => Empty
            }))
        }
    }
}

fn transform_binary(internal_vec: &mut Vec<Decl>,
                    lhs: &lr_ast::NonTerminal,
                    rhs: &lr_ast::NonTerminal,
                    bop: &BinaryOperation) -> Result<NonTerminal, IrError> {
    let collected = collect_binary(internal_vec, lhs, rhs, bop)?;

    match bop {
        Or => {
            let id = format!("II_OR_EXPAND_{}_II", internal_vec.len());
            for item in collected {
                internal_vec.push(Decl {
                    identifier: id.clone(),
                    maps_to: item,
                })
            }
            Ok(Term(Identifier(id)))
        }
        Concat => Ok(NonTerminal::Concat(collected))
    }
}

fn collect_binary(internal_vec: &mut Vec<Decl>, c_lhs: &lr_ast::NonTerminal, c_rhs: &lr_ast::NonTerminal, c_bop: &BinaryOperation) -> Result<Vec<NonTerminal>, IrError> {
    let mut tmp:Vec<NonTerminal> = vec![];
    collect_binary_helper(&mut tmp, internal_vec, c_lhs, c_bop)?;
    tmp.push(transform_non_terminal(internal_vec, c_rhs)?);


    Ok(tmp)
}

fn collect_binary_helper(ret: &mut Vec<NonTerminal>, internal_vec: &mut Vec<Decl>, item: &lr_ast::NonTerminal, c_bop: &BinaryOperation) -> Result<(), IrError> {
    if let lr_ast::NonTerminal::Binary { lhs, rhs, bop } = item {
        if bop == c_bop {
            ret.append(&mut collect_binary(internal_vec, lhs, rhs, bop)?)
        }
        else {
            ret.push(transform_non_terminal(internal_vec, lhs)?)
        }
    } else {
        ret.push(transform_non_terminal(internal_vec, item)?)
    }
    Ok(())
}

#[cfg(test)]
pub mod test {
    use crate::lr_augmenter::LrAugmenter;
    use crate::lr_ir::{Decl, transform};
    use crate::lr_ir::NonTerminal::{Concat, Term};
    use crate::lr_ir::Terminal::Identifier;

    #[test]
    pub fn simple_or_concat() {
        let g = r#"a -> (b e) | c | d"#;
        let mut generator = LrAugmenter::new(g).unwrap();
        generator.augment_grammar();
        let t = transform(&generator.decls).unwrap();
        println!("simple or and concat collapse: {:#?}", t)
    }

    #[test]
    pub fn simple_or_collapse() {
        let g = r#"a -> b | c | d"#;
        let mut generator = LrAugmenter::new(g).unwrap();
        generator.augment_grammar();
        let t = transform(&generator.decls).unwrap();
        println!("simple or collapse: {:#?}", t)
    }

    #[test]
    pub fn simple_concat_collapse() {
        let g = r#"a -> b c d"#;
        let mut generator = LrAugmenter::new(g).unwrap();
        generator.augment_grammar();
        let t = transform(&generator.decls).unwrap();
        println!("simple concat collapse: {:#?}", t)
    }

    #[test]
    pub fn initial_test() {
        let g = r#"decl         -> IDENTIFIER "->" nonterminal
nonterminal  -> terminal
			  | nonterminal "|" nonterminal
			  | nonterminal nonterminal
			  | nonterminal "+"
			  | nonterminal "*"
			  | nonterminal "?"
			  | "\(" nonterminal "\)"
terminal     -> REGEX
			  | IDENTIFIER"#;
        let mut generator = LrAugmenter::new(g).unwrap();
        generator.augment_grammar();
        let t = transform(&generator.decls).unwrap();
        println!("initial_test: {:#?}", t)
    }
}