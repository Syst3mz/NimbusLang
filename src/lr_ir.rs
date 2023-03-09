use regex::Regex;
use crate::lr_ast;
use crate::lr_ast::{BinaryOperation, NonTerminal, Terminal};
use crate::lr_ast::NonTerminal::{Binary, Unary};
use crate::lr_ir::BinaryOperation::{Concat, Or};
use crate::lr_ir::IrError::IllegalUnary;
use crate::lr_ir::NonTerminal::Term;
use crate::lr_ir::Terminal::{Empty, Identifier, StringLiteral};

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Decl {
    pub(crate) identifier: String,
    pub(crate) maps_to: Vec<Terminal>
}


#[derive(Debug)]
pub(crate) enum IrError {
    IllegalUnary(lr_ast::NonTerminal)
}

pub(crate) struct IrTransformer {
    decls: Vec<Decl>
}

impl IrTransformer {
    pub(crate) fn new() -> Self {
        Self {
            decls: vec![],
        }
    }
    pub(crate) fn transform_decls(&mut self, input: Vec<lr_ast::Decl>) -> Result<(), IrError> {
        for decl in input {
            self.transform(decl)?;
        }
        Ok(())
    }
    fn transform(&mut self, decl: lr_ast::Decl) -> Result<(), IrError> {
        self.transform_decl(decl)?;
        Ok(())
    }
    fn transform_decl(&mut self, decl: lr_ast::Decl) -> Result<(), IrError> {
        /*match decl.maps_to {
            NonTerminal::Binary { lhs, rhs, bop } => {
                self.transform_binary(decl.identifier, *lhs, *rhs, bop)?
            }
            NonTerminal::Unary { ref uop, ref lhs } => Err(IllegalUnary(nt.clone())),
            Term(t) => self.decls.push(Decl {
                identifier: decl.identifier,
                maps_to: vec![t],
            })
        }*/


        Ok(())
    }
    fn transform_binary(&mut self, name: String, lhs: NonTerminal, rhs: NonTerminal, bop: BinaryOperation) -> Result<Vec<Terminal>, IrError> {
        let collected = self.collect_binary(lhs, rhs, bop)?;
        /*match bop {
            Or => {}
            Concat => {}
        }*/
        Ok(vec![Terminal::Empty])
    }

    fn collect_binary_helper(&self, nt: NonTerminal, p_bop: BinaryOperation) -> Result<Vec<lr_ast::NonTerminal>, IrError> {
        let mut collected:Vec<NonTerminal> = vec![];
        if let Binary { lhs, rhs, bop } = nt.clone() {
            if bop == p_bop {
                collected.append(&mut self.collect_binary(*lhs, *rhs, bop)?)
            }
            else {
                collected.push(nt);
            }
        }
        else {
            collected.push(nt);
        }

        Ok(collected)
    }

    fn collect_binary(&self, lhs: NonTerminal, rhs: NonTerminal, bop: BinaryOperation) -> Result<Vec<lr_ast::NonTerminal>, IrError> {
        let mut collected = self.collect_binary_helper(lhs, bop.clone())?;
        collected.append(&mut self.collect_binary_helper(rhs, bop)?);

        return Ok(collected);
    }
}


#[cfg(test)]
pub mod test {
    use crate::lr_ast::BinaryOperation;
    use crate::lr_ast::NonTerminal::Binary;
    use crate::lr_ir::IrTransformer;
    use crate::lr_ir::NonTerminal::{Term};
    use crate::lr_ir::Terminal::Identifier;


    #[test]
    fn test_homogenous_binary_collection() {
        let transformer:IrTransformer = IrTransformer::new();
        let r = transformer.collect_binary(
            Binary {
                lhs: Box::new(Term(Identifier("a".to_string()))),
                rhs: Box::new(Term(Identifier("b".to_string()))),
                bop: BinaryOperation::Or,
            },
            Term(Identifier("c".to_string())),
            BinaryOperation::Or
        ).unwrap();
        assert_eq!(r, vec![
            Term(Identifier("a".to_string())),
            Term(Identifier("b".to_string())),
            Term(Identifier("c".to_string()))
        ])
    }

    #[test]
    fn test_homogenous_binary_collection_3_ply() {
        let transformer:IrTransformer = IrTransformer::new();
        let r = transformer.collect_binary(
            Binary {
                lhs: Box::new(Binary {
                    lhs: Box::new(Term(Identifier("a".to_string()))),
                    rhs: Box::new(Term(Identifier("b".to_string()))),
                    bop: BinaryOperation::Or,
                }),
                rhs: Box::new(Term(Identifier("c".to_string()))),
                bop: BinaryOperation::Or,
            },
            Term(Identifier("d".to_string())),
            BinaryOperation::Or
        ).unwrap();
        assert_eq!(r, vec![
            Term(Identifier("a".to_string())),
            Term(Identifier("b".to_string())),
            Term(Identifier("c".to_string())),
            Term(Identifier("d".to_string())),
        ])
    }

    fn test_homogenous_binary_collection_4_ply() {
        let transformer:IrTransformer = IrTransformer::new();
        let r = transformer.collect_binary(
            Binary {
                lhs: Box::new(Binary {
                    lhs: Box::new(Binary {
                        lhs: Box::new(Term(Identifier("a".to_string()))),
                        rhs: Box::new(Term(Identifier("b".to_string()))),
                        bop: BinaryOperation::Or,
                    }),
                    rhs: Box::new(Term(Identifier("c".to_string()))),
                    bop: BinaryOperation::Or,
                }),
                rhs: Box::new(Term(Identifier("d".to_string()))),
                bop: BinaryOperation::Or,
            },
            Term(Identifier("e".to_string())),
            BinaryOperation::Or
        ).unwrap();
        assert_eq!(r, vec![
            Term(Identifier("a".to_string())),
            Term(Identifier("b".to_string())),
            Term(Identifier("c".to_string())),
            Term(Identifier("d".to_string())),
            Term(Identifier("e".to_string())),
        ])
    }

    #[test]
    fn test_heterogenous_binary_collection_3_ply() {
        let transformer:IrTransformer = IrTransformer::new();
        let r = transformer.collect_binary(
            Binary {
                lhs: Box::new(Binary {
                    lhs: Box::new(Term(Identifier("a".to_string()))),
                    rhs: Box::new(Term(Identifier("b".to_string()))),
                    bop: BinaryOperation::Concat,
                }),
                rhs: Box::new(Term(Identifier("c".to_string()))),
                bop: BinaryOperation::Or,
            },
            Term(Identifier("d".to_string())),
            BinaryOperation::Or
        ).unwrap();
        assert_eq!(r, vec![
            Binary {
                    lhs: Box::new(Term(Identifier("a".to_string()))),
                    rhs: Box::new(Term(Identifier("b".to_string()))),
                    bop: BinaryOperation::Concat,
                },
            Term(Identifier("c".to_string())),
            Term(Identifier("d".to_string())),
        ])
    }

    /*#[test]
    fn test_heterogenous_binary_collection_3_ply_internal() {
        let transformer: IrTransformer = IrTransformer::new();
        let r = transformer.collect_binary(
            Binary {
                lhs:,
                rhs: Box::new(Term(Identifier("c".to_string()))),
                bop: BinaryOperation::Concat,
            },
            Term(Identifier("d".to_string())),
            BinaryOperation::Or
        ).unwrap();
        assert_eq!(r, vec![
            Binary {
                lhs: Box::new(Term(Identifier("a".to_string()))),
                rhs: Box::new(Term(Identifier("b".to_string()))),
                bop: BinaryOperation::Concat,
            },
            Term(Identifier("c".to_string())),
            Term(Identifier("d".to_string())),
        ])
    }*/
}