use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::{Deref, DerefMut};
use regex::{Error, Regex};
use crate::lr_ir::Terminal::{Empty, Identifier, StringLiteral};
use crate::{lr_ast, lr_or_remover};
use crate::lr_or_remover::NonTerminal;


struct UsefulRegex(Regex);

impl UsefulRegex {
    fn new(from: &str) -> Result<UsefulRegex, Error> {
        let regex = Regex::new(from);
        match regex {
            Ok(r) => {Ok(UsefulRegex::from(r))}
            Err(e) => {Err(e)}
        }
    }
}

impl From<Regex> for UsefulRegex {
    fn from(value: Regex) -> Self {
        Self {
            0: value,
        }
    }
}

impl Deref for UsefulRegex {
    type Target = Regex;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for UsefulRegex {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl PartialEq for UsefulRegex {
    fn eq(&self, other: &Self) -> bool {
        self.to_string() == other.to_string()
    }
}

impl Hash for UsefulRegex {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.to_string().hash(state)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Decl {
    pub(crate) identifier: String,
    pub(crate) maps_to: Vec<Terminal>
}

#[derive(Debug, Clone)]
pub(crate) enum Terminal {
    StringLiteral(UsefulRegex),
    Identifier(String),
    Empty
}

impl Hash for Terminal {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            StringLiteral(s) => s.to_string().hash(state),
            Identifier(i) => { i.hash(state)}
            Empty => {state.write_u64(0)}
        }
    }
}

impl ToString for Terminal {
    fn to_string(&self) -> String {
        match self {
            StringLiteral(s) => {s.to_string()}
            Identifier(id) => {String::from(id)}
            Empty => {String::from("ϵ")}
        }
    }
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
        NonTerminal::Term(t) => match t {
            lr_ast::Terminal::Identifier(i) => {vec![Identifier(i)]}
            lr_ast::Terminal::StringLiteral(r) => {vec![StringLiteral(UsefulRegex::from(r))]}
            lr_ast::Terminal::Empty => {vec![Empty]}
        }
    }
}

#[cfg(test)]
pub mod test {
    use regex::Regex;
    use crate::lr_ir::{Decl, Terminal, to_ir, UsefulRegex};
    use crate::{lr_or_remover, lr_unary_remover};
    use crate::lr_ir::Terminal::StringLiteral;
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
                identifier: "E_PRIME".to_string(),
                maps_to: vec![
                    Terminal::Identifier("a".to_string()),
                    Terminal::Identifier("b".to_string()),
                ],
            },
            Decl {
                identifier: "E_PRIME".to_string(),
                maps_to: vec![
                    Terminal::Identifier("c".to_string()),
                    Terminal::Identifier("d".to_string()),
                ],
            },
            Decl {
                identifier: "E".to_string(),
                maps_to: vec![Terminal::Identifier("E_PRIME".to_string())],
            },
            Decl {
                identifier: "II_INITIAL_PRODUCTION_II".to_string(),
                maps_to: vec![Terminal::Identifier("E".to_string())],
            }
        ])
    }

    #[test]
    fn full_pipeline_check() {
        let dat = build_testing_data(lr_unary_remover::tests::WRONG_SELF_G);
        assert_eq!(to_ir(dat), [
            Decl {
                identifier: "decl".to_string(),
                maps_to: vec![
                    Terminal::Identifier(
                        "IDENTIFIER".to_string(),
                    ),
                    StringLiteral(
                        UsefulRegex::new("->").unwrap(),
                    ),
                    Terminal::Identifier(
                        "nonterminal".to_string(),
                    ),
                ],
            },
            Decl {
                identifier: "nonterminal_PRIME".to_string(),
                maps_to: vec![
                    Terminal::Identifier(
                        "terminal".to_string(),
                    ),
                ],
            },
            Decl {
                identifier: "nonterminal_PRIME".to_string(),
                maps_to: vec![
                    Terminal::Identifier(
                        "nonterminal".to_string(),
                    ),
                    StringLiteral(
                        UsefulRegex::new(r"\|").unwrap(),
                    ),
                    Terminal::Identifier(
                        "nonterminal".to_string(),
                    ),
                ],
            },
            Decl {
                identifier: "nonterminal_PRIME".to_string(),
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
                identifier: "nonterminal_PRIME".to_string(),
                maps_to: vec![
                    Terminal::Identifier(
                        "nonterminal".to_string(),
                    ),
                    StringLiteral(
                        UsefulRegex::new(r"\+").unwrap(),
                    ),
                ],
            },
            Decl {
                identifier: "nonterminal_PRIME".to_string(),
                maps_to: vec![
                    Terminal::Identifier(
                        "nonterminal".to_string(),
                    ),
                    StringLiteral(
                        UsefulRegex::new(r"\*").unwrap(),
                    ),
                ],
            },
            Decl {
                identifier: "nonterminal_PRIME".to_string(),
                maps_to: vec![
                    Terminal::Identifier(
                        "nonterminal".to_string(),
                    ),
                    StringLiteral(
                        UsefulRegex::new(r"\?").unwrap(),
                    ),
                ],
            },
            Decl {
                identifier: "nonterminal_PRIME".to_string(),
                maps_to: vec![
                    StringLiteral(
                        UsefulRegex::new(r"\(").unwrap(),
                    ),
                    Terminal::Identifier(
                        "nonterminal".to_string(),
                    ),
                    StringLiteral(
                        UsefulRegex::new(r"\)").unwrap(),
                    ),
                ],
            },
            Decl {
                identifier: "nonterminal".to_string(),
                maps_to: vec![
                    Terminal::Identifier(
                        "nonterminal_PRIME".to_string(),
                    ),
                ],
            },
            Decl {
                identifier: "terminal_PRIME".to_string(),
                maps_to: vec![
                    Terminal::Identifier(
                        "REGEX".to_string(),
                    ),
                ],
            },
            Decl {
                identifier: "terminal_PRIME".to_string(),
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
                        "terminal_PRIME".to_string(),
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