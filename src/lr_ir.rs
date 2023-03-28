use crate::lr_ast::Terminal;
use crate::lr_or_remover;
use crate::lr_or_remover::NonTerminal;

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Decl {
    pub(crate) identifier: String,
    pub(crate) maps_to: Vec<Terminal>
}

pub(crate) fn to_ir(input: Vec<lr_or_remover::Decl>) -> Vec<Decl> {
    let mut ret: Vec<Decl> = vec![];

    for decl in input {
        let transformed = transform(decl.maps_to);
        ret.push(Decl {
            identifier: decl.identifier.clone(),
            maps_to: transformed,
        });
    }

    ret
}

fn transform(nt: NonTerminal) -> Vec<Terminal> {
    match nt {
        NonTerminal::Concat { lhs, rhs } => {
            let mut ret = transform(*lhs);
            ret.append(&mut transform(*rhs));

            ret
        }
        NonTerminal::Term(t) => vec![t]
    }
}

#[cfg(test)]
pub mod test {
    use regex::Regex;
    use crate::lr_ast::Terminal;
    use crate::lr_ir::{Decl, to_ir};
    use crate::{lr_or_remover, lr_unary_remover};
    use crate::lr_ast::Terminal::StringLiteral;
    use crate::lr_or_remover::remove_or;
    use crate::lr_parser::LRParser;
    use crate::lr_starting_production_adder::augment_grammar_with_starting_production;
    use crate::lr_unary_remover::remove_unary;

    fn build_testing_data(input: &str) -> Vec<lr_or_remover::Decl>{
        let parsed = LRParser::new(input).parse().unwrap();
        let mut parsed = remove_unary(parsed);
        augment_grammar_with_starting_production(&mut parsed);
        let parsed = remove_or(parsed);
        return parsed;
    }

    #[test]
    fn simple_flatten() {
        assert_eq!(to_ir(build_testing_data("E -> a b c")), vec![
            Decl {
                identifier: "E".to_string(),
                maps_to: vec![
                    Terminal::Identifier("a".to_string()),
                    Terminal::Identifier("b".to_string()),
                    Terminal::Identifier("c".to_string())
                ],
            },
            Decl {
                identifier: "II_INITIAL_PRODUCTION_II".to_string(),
                maps_to: vec![Terminal::Identifier("E".to_string())],
            }
        ])
    }

    #[test]
    fn harder_flatten() {
        assert_eq!(to_ir(build_testing_data("E -> a b | c d")), vec![
            Decl {
                identifier: "II_E_PRIME_OR_EXPAND_II".to_string(),
                maps_to: vec![
                    Terminal::Identifier("a".to_string()),
                    Terminal::Identifier("b".to_string()),
                ],
            },
            Decl {
                identifier: "II_E_PRIME_OR_EXPAND_II".to_string(),
                maps_to: vec![
                    Terminal::Identifier("c".to_string()),
                    Terminal::Identifier("d".to_string()),
                ],
            },
            Decl {
                identifier: "E".to_string(),
                maps_to: vec![Terminal::Identifier("II_E_PRIME_OR_EXPAND_II".to_string())],
            },
            Decl {
                identifier: "II_INITIAL_PRODUCTION_II".to_string(),
                maps_to: vec![Terminal::Identifier("E".to_string())],
            }
        ])
    }

    #[test]
    fn full_pipeline_check() {
        let dat = build_testing_data(lr_unary_remover::tests::SELF_G);
        assert_eq!(to_ir(dat), [
            Decl {
                identifier: "decl".to_string(),
                maps_to: vec![
                    Terminal::Identifier(
                        "IDENTIFIER".to_string(),
                    ),
                    StringLiteral(
                        Regex::new("->").unwrap(),
                    ),
                    Terminal::Identifier(
                        "nonterminal".to_string(),
                    ),
                ],
            },
            Decl {
                identifier: "II_nonterminal_PRIME_OR_EXPAND_II".to_string(),
                maps_to: vec![
                    Terminal::Identifier(
                        "terminal".to_string(),
                    ),
                ],
            },
            Decl {
                identifier: "II_nonterminal_PRIME_OR_EXPAND_II".to_string(),
                maps_to: vec![
                    Terminal::Identifier(
                        "nonterminal".to_string(),
                    ),
                    StringLiteral(
                        Regex::new(r"\|").unwrap(),
                    ),
                    Terminal::Identifier(
                        "nonterminal".to_string(),
                    ),
                ],
            },
            Decl {
                identifier: "II_nonterminal_PRIME_OR_EXPAND_II".to_string(),
                maps_to: vec![
                    Terminal::Identifier(
                        "nonterminal".to_string(),
                    ),
                    Terminal::Identifier(
                        "nonterminal".to_string(),
                    ),
                ],
            },
            Decl {
                identifier: "II_nonterminal_PRIME_OR_EXPAND_II".to_string(),
                maps_to: vec![
                    Terminal::Identifier(
                        "nonterminal".to_string(),
                    ),
                    StringLiteral(
                        Regex::new(r"\+").unwrap(),
                    ),
                ],
            },
            Decl {
                identifier: "II_nonterminal_PRIME_OR_EXPAND_II".to_string(),
                maps_to: vec![
                    Terminal::Identifier(
                        "nonterminal".to_string(),
                    ),
                    StringLiteral(
                        Regex::new(r"\*").unwrap(),
                    ),
                ],
            },
            Decl {
                identifier: "II_nonterminal_PRIME_OR_EXPAND_II".to_string(),
                maps_to: vec![
                    Terminal::Identifier(
                        "nonterminal".to_string(),
                    ),
                    StringLiteral(
                        Regex::new(r"\?").unwrap(),
                    ),
                ],
            },
            Decl {
                identifier: "II_nonterminal_PRIME_OR_EXPAND_II".to_string(),
                maps_to: vec![
                    StringLiteral(
                        Regex::new(r"\(").unwrap(),
                    ),
                    Terminal::Identifier(
                        "nonterminal".to_string(),
                    ),
                    StringLiteral(
                        Regex::new(r"\)").unwrap(),
                    ),
                ],
            },
            Decl {
                identifier: "nonterminal".to_string(),
                maps_to: vec![
                    Terminal::Identifier(
                        "II_nonterminal_PRIME_OR_EXPAND_II".to_string(),
                    ),
                ],
            },
            Decl {
                identifier: "II_terminal_PRIME_OR_EXPAND_II".to_string(),
                maps_to: vec![
                    Terminal::Identifier(
                        "REGEX".to_string(),
                    ),
                ],
            },
            Decl {
                identifier: "II_terminal_PRIME_OR_EXPAND_II".to_string(),
                maps_to: vec![
                    Terminal::Identifier(
                        "IDENTIFIER".to_string(),
                    ),
                ],
            },
            Decl {
                identifier: "terminal".to_string(),
                maps_to: vec![
                    Terminal::Identifier(
                        "II_terminal_PRIME_OR_EXPAND_II".to_string(),
                    ),
                ],
            },
            Decl {
                identifier: "II_INITIAL_PRODUCTION_II".to_string(),
                maps_to: vec![
                    Terminal::Identifier(
                        "decl".to_string(),
                    ),
                ],
            },
        ])
    }
}