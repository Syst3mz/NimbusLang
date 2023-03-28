use crate::lr_ast::{BinaryOperation, Terminal, UnaryOperation};
use crate::lr_ast::Terminal::{Empty, Identifier};
use crate::{lr_ast};
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
        let transformed = transform_non_terminal(decl.maps_to, &decl.identifier, &mut ret);
        ret.push(Decl {
            identifier: decl.identifier.clone(),
            maps_to: transformed,
        })
    }

    ret
}


fn transform_non_terminal(nt: lr_ast::NonTerminal, name: &str, ret: &mut Vec<Decl>) -> NonTerminal {
    match nt {
        lr_ast::NonTerminal::Binary { lhs, rhs, bop } => {
            Binary {
                lhs: Box::new(transform_non_terminal(*lhs, name, ret)),
                rhs: Box::new(transform_non_terminal(*rhs, name, ret)),
                bop,
            }
        }
        lr_ast::NonTerminal::Unary { uop, lhs } => transform_unary(uop, *lhs, name, ret),
        lr_ast::NonTerminal::Term(t) => Term(t)
    }
}
fn transform_unary(uop: UnaryOperation, lhs: lr_ast::NonTerminal, name: &str, ret: &mut Vec<Decl>) -> NonTerminal {
    match uop {
        UnaryOperation::Plus => {
            transform_klein_plus(lhs, name, "PLUS_EXPAND", ret)
        }
        UnaryOperation::Star => {
            Binary {
                lhs: Box::new(transform_klein_plus(lhs, name, "STAR_EXPAND", ret)),
                rhs: Box::new(Term(Empty)),
                bop: BinaryOperation::Or,
            }
        }
        UnaryOperation::Optional => Binary {
            lhs: Box::new(transform_non_terminal(lhs, name, ret)),
            rhs: Box::new(Term(Empty)),
            bop: BinaryOperation::Or,
        }
    }
}

fn transform_klein_plus(lhs: lr_ast::NonTerminal, name: &str, message: &str, ret: &mut Vec<Decl>) -> NonTerminal {
    let id = format!("II_{name}_PRIME_{message}_II");
    let lhs = transform_non_terminal(lhs, name, ret);
    ret.push(Decl {
        identifier: id.clone(),
        maps_to: Binary {
            lhs: Box::new(lhs.clone()),
            rhs: Box::new(Binary {
                lhs: Box::new(lhs.clone()),
                rhs: Box::new(Term(Identifier(id.clone()))),
                bop: BinaryOperation::Concat,
            }),
            bop: BinaryOperation::Or,
        },
    });
    Term(Identifier(id))
}

#[cfg(test)]
pub mod tests {
    use regex::Regex;
    use crate::lr_ast::BinaryOperation;
    use crate::lr_ast::BinaryOperation::{Concat, Or};
    use crate::lr_ast::Terminal::{Empty, Identifier, StringLiteral};
    use crate::lr_parser::LRParser;
    use crate::lr_unary_remover::{Decl, remove_unary};
    use crate::lr_unary_remover::NonTerminal::{Binary, Term};

    pub static SELF_G: &str = r#"decl         -> IDENTIFIER "->" nonterminal
nonterminal  -> terminal
			  | nonterminal "\|" nonterminal
			  | nonterminal nonterminal
			  | nonterminal "\+"
			  | nonterminal "\*"
			  | nonterminal "\?"
			  | "\(" nonterminal "\)"
terminal     -> REGEX
			  | IDENTIFIER"#;

    fn build_testing_data(input: &str) -> Vec<crate::lr_unary_remover::Decl> {
        let parsed = LRParser::new(input).parse().unwrap();
        return remove_unary(parsed);
    }

    #[test]
    pub fn test_klein_plus_expansion_harder() {
        let g = r#"E -> "a"+ b"#;
        assert_eq!(build_testing_data(g), vec![
            Decl {
                identifier: "II_E_PRIME_PLUS_EXPAND_II".to_string(),
                maps_to: Binary {
                    lhs: Box::new(Term(StringLiteral(Regex::new("a").unwrap()))),
                    rhs: Box::new(Binary {
                        lhs: Box::new(Term(StringLiteral(Regex::new("a").unwrap()))),
                        rhs: Box::new(Term(Identifier("II_E_PRIME_PLUS_EXPAND_II".to_string()))),
                        bop: Concat,
                    }),
                    bop: Or,
                },
            },
            Decl {
                identifier: "E".to_string(),
                maps_to: Binary {
                    lhs: Box::new(Term(Identifier("II_E_PRIME_PLUS_EXPAND_II".to_string()))),
                    rhs: Box::new(Term(Identifier("b".to_string()))),
                    bop: Concat,
                },
            },
        ])
    }

    #[test]
    pub fn test_paren_work() {
        let g = r#"E -> (a b)"#;
        assert_eq!(build_testing_data(g), vec![
            Decl {
                identifier: "E".to_string(),
                maps_to: Binary {
                    lhs: Box::new(Term(Identifier("a".to_string()))),
                    rhs: Box::new(Term(Identifier("b".to_string()))),
                    bop: BinaryOperation::Concat,
                },
            },
        ])
    }

    #[test]
    pub fn test_paren_work_harder() {
        let g = r#"E -> (a b) | c "#;
        assert_eq!(build_testing_data(g), vec![
            Decl {
                identifier: "E".to_string(),
                maps_to: Binary {
                    lhs: Box::new(Binary {
                        lhs: Box::new(Term(Identifier("a".to_string()))),
                        rhs: Box::new(Term(Identifier("b".to_string()))),
                        bop: BinaryOperation::Concat,
                    }),
                    rhs: Box::new(Term(Identifier("c".to_string()))),
                    bop: BinaryOperation::Or,
                },
            },
        ])
    }

    #[test]
    pub fn test_klein_plus_expansion() {
        let g = r#"E -> "a"+"#;
        assert_eq!(build_testing_data(g), vec![
            Decl {
                identifier: "II_E_PRIME_PLUS_EXPAND_II".to_string(),
                maps_to: Binary {
                    lhs: Box::new(Term(StringLiteral(Regex::new("a").unwrap()))),
                    rhs: Box::new(Binary {
                        lhs: Box::new(Term(StringLiteral(Regex::new("a").unwrap()))),
                        rhs: Box::new(Term(Identifier("II_E_PRIME_PLUS_EXPAND_II".to_string()))),
                        bop: Concat,
                    }),
                    bop: Or,
                },
            },
            Decl {
                identifier: "E".to_string(),
                maps_to: Term(Identifier("II_E_PRIME_PLUS_EXPAND_II".to_string())),
            }
        ])
    }

    #[test]
    pub fn test_klein_star_expansion() {
        let g = r#"E -> "a"*"#;
        assert_eq!(build_testing_data(g), vec![
            Decl {
                identifier: "II_E_PRIME_STAR_EXPAND_II".to_string(),
                maps_to: Binary {
                    lhs: Box::new(Term(
                        StringLiteral(
                            Regex::new("a").unwrap(),
                        ),
                    )),
                    rhs: Box::new(Binary {
                        lhs: Box::new(Term(
                            StringLiteral(
                                Regex::new("a").unwrap(),
                            ),
                        )),
                        rhs: Box::new(Term(
                            Identifier(
                                "II_E_PRIME_STAR_EXPAND_II".to_string(),
                            ),
                        )),
                        bop: Concat,
                    }),
                    bop: Or,
                },
            },
            Decl {
                identifier: "E".to_string(),
                maps_to: Binary {
                    lhs: Box::new(Term(Identifier("II_E_PRIME_STAR_EXPAND_II".to_string()))),
                    rhs: Box::new(Term(Empty)),
                    bop: Or,
                },
            },
        ])
    }

    #[test]
    pub fn test_optional_expansion() {
        let g = r#"E -> "a"?"#;
        assert_eq!(build_testing_data(g), vec![
            Decl {
                identifier: "E".to_string(),
                maps_to: Binary {
                    lhs: Box::new(Term(StringLiteral(Regex::new("a").unwrap()))),
                    rhs: Box::new(Term(Empty)),
                    bop: Or
                },
            }
        ])
    }
}