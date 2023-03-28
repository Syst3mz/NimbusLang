use crate::lr_ast::{BinaryOperation, Terminal};
use crate::lr_ast::BinaryOperation::{Or, Concat};
use crate::lr_ast::Terminal::Identifier;
use crate::lr_or_remover::NonTerminal::Term;
use crate::lr_unary_remover;
use crate::lr_unary_remover::NonTerminal::Binary;

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Decl {
    pub(crate) identifier: String,
    pub(crate) maps_to: NonTerminal
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum NonTerminal {
    Concat {
        lhs: Box<NonTerminal>,
        rhs: Box<NonTerminal>,
    },

    Term(Terminal)
}

pub(crate) fn remove_or(input: Vec<lr_unary_remover::Decl>) -> Vec<Decl> {
    let mut ret: Vec<Decl> = vec![];
    for decl in input {
        let transformed = transform_nt(decl.maps_to, &decl.identifier, &mut ret);
        ret.push(Decl {
            identifier: decl.identifier,
            maps_to: transformed,
        })
    }

    ret
}

fn transform_nt(nt: lr_unary_remover::NonTerminal, name: &str, ret: &mut Vec<Decl>, ) -> NonTerminal {
    match nt {
        Binary { lhs, rhs, bop: Concat } => {
            NonTerminal::Concat {
                lhs: Box::new(transform_nt(*lhs, name, ret)),
                rhs: Box::new(transform_nt(*rhs, name, ret)),
            }
        }
        Binary { lhs, rhs, bop: Or } => {
            let collected = collect_binary(*lhs, *rhs, Or);
            let transformed: Vec<NonTerminal> =
                collected.into_iter().map(|x| transform_nt(x, name, ret)).collect();
            let id = format!("II_{name}_PRIME_OR_EXPAND_II");
            for ct in transformed {
                ret.push(Decl {
                    identifier: id.clone(),
                    maps_to: ct,
                })
            }
            Term(Identifier(id))
        }
        lr_unary_remover::NonTerminal::Term(t) => {
            Term(t)
        }
    }
}

fn collect_binary_helper(nt: lr_unary_remover::NonTerminal, p_bop: BinaryOperation) -> Vec<lr_unary_remover::NonTerminal> {
    let mut collected:Vec<lr_unary_remover::NonTerminal> = vec![];
    if let Binary { lhs, rhs, bop } = nt.clone() {
        if bop == p_bop {
            collected.append(&mut collect_binary(*lhs, *rhs, bop))
        }
        else {
            collected.push(nt);
        }
    }
    else {
        collected.push(nt);
    }

    collected
}

fn collect_binary(lhs: lr_unary_remover::NonTerminal, rhs: lr_unary_remover::NonTerminal, bop: BinaryOperation) -> Vec<lr_unary_remover::NonTerminal> {
    let mut collected = collect_binary_helper(lhs, bop.clone());
    collected.append(&mut collect_binary_helper(rhs, bop.clone()));

    return collected;
}

#[cfg(test)]
mod test {
    use crate::lr_ast::Terminal::Identifier;
    use crate::lr_or_remover::{Decl, remove_or};
    use crate::lr_or_remover::NonTerminal::{Concat, Term};
    use crate::{lr_unary_remover};
    use crate::lr_parser::LRParser;
    use crate::lr_starting_production_adder::augment_grammar_with_starting_production;
    use crate::lr_unary_remover::remove_unary;

    fn build_testing_data(input: &str) -> Vec<lr_unary_remover::Decl>{
        let parsed = LRParser::new(input).parse().unwrap();
        let mut parsed = remove_unary(parsed);
        augment_grammar_with_starting_production(&mut parsed);
        return parsed;
    }

    #[test]
    pub fn check_no_or_easy() {
        assert_eq!(remove_or(build_testing_data("E -> A | B")), vec![
            Decl {
                identifier: "II_E_PRIME_OR_EXPAND_II".to_string(),
                maps_to: Term(
                    Identifier(
                        "A".to_string(),
                    ),
                ),
            },
            Decl {
                identifier: "II_E_PRIME_OR_EXPAND_II".to_string(),
                maps_to: Term(
                    Identifier(
                        "B".to_string(),
                    ),
                ),
            },

            Decl {
                identifier: "E".to_string(),
                maps_to: Term(
                    Identifier(
                        "II_E_PRIME_OR_EXPAND_II".to_string(),
                    ),
                ),
            },
            Decl {
                identifier: "II_INITIAL_PRODUCTION_II".to_string(),
                maps_to: Term(
                    Identifier(
                        "E".to_string(),
                    ),
                ),
            },
        ])
    }

    #[test]
    pub fn check_no_or_easy_3zy() {
        assert_eq!(remove_or(build_testing_data("E -> A | B | C")), vec![
            Decl {
                identifier: "II_E_PRIME_OR_EXPAND_II".to_string(),
                maps_to: Term(
                    Identifier(
                        "A".to_string(),
                    ),
                ),
            },
            Decl {
                identifier: "II_E_PRIME_OR_EXPAND_II".to_string(),
                maps_to: Term(
                    Identifier(
                        "B".to_string(),
                    ),
                ),
            },
            Decl {
                identifier: "II_E_PRIME_OR_EXPAND_II".to_string(),
                maps_to: Term(
                    Identifier(
                        "C".to_string(),
                    ),
                ),
            },

            Decl {
                identifier: "E".to_string(),
                maps_to: Term(
                    Identifier(
                        "II_E_PRIME_OR_EXPAND_II".to_string(),
                    ),
                ),
            },
            Decl {
                identifier: "II_INITIAL_PRODUCTION_II".to_string(),
                maps_to: Term(
                    Identifier(
                        "E".to_string(),
                    ),
                ),
            },
        ])
    }

    #[test]
    pub fn check_no_or_concat_and_or() {
        assert_eq!(remove_or(build_testing_data("E -> A B C | D")), vec![
            Decl {
            identifier: "II_E_PRIME_OR_EXPAND_II".to_string(),
            maps_to: Concat {
                    lhs: Box::new(Concat {
                        lhs: Box::new(Term(Identifier("A".to_string()))),
                        rhs: Box::new(Term(Identifier("B".to_string())))
                    }),
                    rhs: Box::new(Term(Identifier("C".to_string()))) }
            },
            Decl {
                identifier: "II_E_PRIME_OR_EXPAND_II".to_string(),
                maps_to: Term(Identifier("D".to_string()))
            },
            Decl {
                identifier: "E".to_string(),
                maps_to: Term(Identifier("II_E_PRIME_OR_EXPAND_II".to_string()))
            },
            Decl {
                identifier: "II_INITIAL_PRODUCTION_II".to_string(),
                maps_to: Term(Identifier("E".to_string())) }])
    }

    #[test]
    fn check_strange_parens() {
        let dat = build_testing_data("E -> A | (B | C)");
        assert_eq!(remove_or(dat), vec![
            Decl {
                identifier: "II_E_PRIME_OR_EXPAND_II".to_string(),
                maps_to: Term(Identifier("A".to_string()))
            },
            Decl {
                identifier: "II_E_PRIME_OR_EXPAND_II".to_string(),
                maps_to: Term(Identifier("B".to_string()))
            },
            Decl {
                identifier: "II_E_PRIME_OR_EXPAND_II".to_string(),
                maps_to: Term(Identifier("C".to_string()))
            },
            Decl {
                identifier: "E".to_string(),
                maps_to: Term(Identifier("II_E_PRIME_OR_EXPAND_II".to_string()))
            },
            Decl {
                identifier: "II_INITIAL_PRODUCTION_II".to_string(),
                maps_to: Term(Identifier("E".to_string()))
            }])
    }
}