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
        transform_decl(&mut ret, decl)
    }

    ret
}

fn transform_decl(arr: &mut Vec<Decl>, decl: lr_unary_remover::Decl) {
    let transformed = transform_nt(arr, decl.maps_to);
    arr.push(Decl {
        identifier: decl.identifier,
        maps_to: transformed,
    })
}

fn transform_nt(arr: &mut Vec<Decl>, nt: lr_unary_remover::NonTerminal) -> NonTerminal {
    match nt {
        Binary { lhs, rhs, bop } => {
            if bop == Concat {
                return NonTerminal::Concat {
                    lhs: Box::new(transform_nt(arr, *lhs)),
                    rhs: Box::new(transform_nt(arr, *rhs)),
                }
            }

            let collected = collect_binary(*lhs, *rhs, Or);
            let transformed:Vec<NonTerminal> =
                collected.into_iter().map(|x| transform_nt(arr, x)).collect();
            let id = format!("II_OR_EXPAND_{}_II", arr.len()+1);
            for ct in transformed {
                arr.push(Decl {
                    identifier: id.clone(),
                    maps_to: ct,
                })
            }
            return Term(Identifier(id));
        }
        lr_unary_remover::NonTerminal::Term(t) => {
            return Term(t);
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
    collected.append(&mut collect_binary_helper(rhs, bop));

    return collected;
}

#[cfg(test)]
mod test {
    use crate::lr_ast::Terminal::Identifier;
    use crate::lr_augmenter::LrAugmenter;
    use crate::lr_or_remover::{Decl, remove_or};
    use crate::lr_or_remover::NonTerminal::{Concat, Term};
    use crate::lr_unary_remover::remove_unary;

    #[test]
    pub fn check_no_or_easy() {
        let mut generator = LrAugmenter::new("E -> A | B").unwrap();
        generator.augment_grammar();
        generator.augment_grammar_with_starting_production();
        let u_free = remove_unary(generator.decls);
        assert_eq!(remove_or(u_free), vec![
            Decl {
                identifier: "II_OR_EXPAND_1_II".to_string(),
                maps_to: Term(
                    Identifier(
                        "A".to_string(),
                    ),
                ),
            },
            Decl {
                identifier: "II_OR_EXPAND_1_II".to_string(),
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
                        "II_OR_EXPAND_1_II".to_string(),
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
        let mut generator = LrAugmenter::new("E -> A | B | C").unwrap();
        generator.augment_grammar();
        generator.augment_grammar_with_starting_production();
        let u_free = remove_unary(generator.decls);
        assert_eq!(remove_or(u_free), vec![
            Decl {
                identifier: "II_OR_EXPAND_1_II".to_string(),
                maps_to: Term(
                    Identifier(
                        "A".to_string(),
                    ),
                ),
            },
            Decl {
                identifier: "II_OR_EXPAND_1_II".to_string(),
                maps_to: Term(
                    Identifier(
                        "B".to_string(),
                    ),
                ),
            },
            Decl {
                identifier: "II_OR_EXPAND_1_II".to_string(),
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
                        "II_OR_EXPAND_1_II".to_string(),
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
        let mut generator = LrAugmenter::new("E -> A B C | D").unwrap();
        generator.augment_grammar();
        generator.augment_grammar_with_starting_production();
        let u_free = remove_unary(generator.decls);
        assert_eq!(remove_or(u_free), vec![
            Decl {
            identifier: "II_OR_EXPAND_1_II".to_string(),
            maps_to: Concat {
                    lhs: Box::new(Concat {
                        lhs: Box::new(Term(Identifier("A".to_string()))),
                        rhs: Box::new(Term(Identifier("B".to_string())))
                    }),
                    rhs: Box::new(Term(Identifier("C".to_string()))) }
            },
            Decl {
                identifier: "II_OR_EXPAND_1_II".to_string(),
                maps_to: Term(Identifier("D".to_string()))
            },
            Decl {
                identifier: "E".to_string(),
                maps_to: Term(Identifier("II_OR_EXPAND_1_II".to_string()))
            },
            Decl {
                identifier: "II_INITIAL_PRODUCTION_II".to_string(),
                maps_to: Term(Identifier("E".to_string())) }])
    }
    #[test]
    fn check_compounded() {
        let mut generator = LrAugmenter::new("E -> (A | B) | (C | D)").unwrap();
        generator.augment_grammar();
        generator.augment_grammar_with_starting_production();
        let u_free = remove_unary(generator.decls);
        assert_eq!(remove_or(u_free), vec![
            Decl {
                identifier: "II_OR_EXPAND_1_II".to_string(),
                maps_to: Term(Identifier("A".to_string()))
            },
            Decl {
                identifier: "II_OR_EXPAND_1_II".to_string(),
                maps_to: Term(Identifier("B".to_string()))
            },
            Decl {
                identifier: "II_OR_EXPAND_2_II".to_string(),
                maps_to: Term(Identifier("C".to_string()))
            },
            Decl {
                identifier: "II_OR_EXPAND_2_II".to_string(),
                maps_to: Term(Identifier("D".to_string()))
            },
            Decl {
                identifier: "II_OR_EXPAND_3_II".to_string(),
                maps_to: Term(Identifier("II_OR_EXPAND_1_II".to_string()))
            },
            Decl {
                identifier: "II_OR_EXPAND_3_II".to_string(),
                maps_to: Term(Identifier("II_OR_EXPAND_2_II".to_string()))
            },
            Decl {
                identifier: "E".to_string(),
                maps_to: Term(Identifier("II_OR_EXPAND_3_II".to_string()))
            },
            Decl {
                identifier: "II_INITIAL_PRODUCTION_II".to_string(),
                maps_to: Term(Identifier("E".to_string()))
            }])
    }
}