use std::collections::{HashMap, HashSet};
use std::fmt::format;
use std::hash::Hash;
use std::io::empty;
use crate::lr_ast::{BinaryOperation, Decl, NonTerminal, Terminal, UnaryOperation};
use crate::lr_ast::NonTerminal::{Binary, Term, Unary};
use crate::lr_ast::Terminal::{Empty, Identifier};
use crate::lr_augmenter::GeneratorErr::ParserErr;
use crate::{lr_ast, lr_parser};
use crate::lr_lexer::LRToken::Or;
use crate::lr_parser::{LRParser};

#[derive(Debug)]
pub(crate) enum GeneratorErr {
    ParserErr(lr_parser::ParserErr)
}

impl From<lr_parser::ParserErr> for GeneratorErr {
    fn from(a: lr_parser::ParserErr) -> Self {
        ParserErr(a)
    }
}

#[derive(Debug)]
pub(crate) struct LrAugmenter {
    pub decls: Vec<Decl>
}

impl LrAugmenter {
    pub fn new(text: &str) -> Result<Self, GeneratorErr> {
        let mappings:Vec<(String, Decl)> = LRParser::new(text)
            .parse()?
            .iter()
            .map(|x| (x.identifier.to_owned(), x.clone()))
            .collect();
        let mut backing_vec = Vec::new();

        for (name, decl) in mappings {
            backing_vec.push(decl);
        }

        Ok(Self { decls: backing_vec } )
    }

    pub(crate) fn augment_grammar(&mut self) {
        let mut idx = 0;
        while idx < self.decls.len() {
            self.decls[idx].maps_to = self.augment_production(self.decls[idx].maps_to.clone());
            idx += 1;
        }
    }


    fn augment_production(&mut self, nt: NonTerminal) -> NonTerminal {
        match nt {
            Binary { lhs, rhs, bop } => {
                Binary {
                    lhs: Box::new(self.augment_production(*lhs)),
                    rhs: Box::new(self.augment_production(*rhs)),
                    bop,
                }
            }
            Unary { uop, lhs } => self.augment_unary(uop, lhs),
            _ => nt
        }
    }
    fn augment_unary(&mut self, uop: UnaryOperation, lhs: Box<NonTerminal>) -> NonTerminal {
        match uop {
            UnaryOperation::Plus => {
                self.augment_klein_plus(lhs)
            }
            UnaryOperation::Star => {
                Binary {
                    lhs: Box::new(self.augment_klein_plus(lhs)),
                    rhs: Box::new(Term(Empty)),
                    bop: BinaryOperation::Or,
                }
            }
            UnaryOperation::Optional => Binary {
                lhs,
                rhs: Box::new(Term(Empty)),
                bop: BinaryOperation::Or,
            }
        }
    }

    fn augment_klein_plus(&mut self, lhs: Box<NonTerminal>) -> NonTerminal {
        let id = format!("II_PLUS_EXPAND_{}_II", self.decls.len()+1);
        let augmented_lhs = self.augment_production(*lhs.clone());
        self.decls.push(Decl {
            identifier: id.clone(),
            maps_to: Binary {
                lhs: Box::new(augmented_lhs.clone()),
                rhs: Box::new(Binary {
                    lhs: Box::new(augmented_lhs.clone()),
                    rhs: Box::new(Term(Identifier(id.clone()))),
                    bop: BinaryOperation::Concat,
                }),
                bop: BinaryOperation::Or,
            },
        });
        Term(Identifier(id))
    }

    fn find_dangling_ids(&self) -> Vec<String> {
        let mut ids_on_right:HashSet<String> = HashSet::new();
        for decl in &self.decls {
            for term in Self::find_ids(&decl.maps_to) {
                ids_on_right.insert(term);
            }
        }

        let mut ids_unused: Vec<String> = vec![];
        for decl in &self.decls {
            if !ids_on_right.contains(&decl.identifier) {
                ids_unused.push(decl.identifier.clone());
            }
        }

        ids_unused
    }
    
    fn find_ids(terminal: &NonTerminal) -> Vec<String> {
        let mut strings: Vec<String> = vec![];
        match terminal {
            Binary { lhs, rhs, bop } => {
                strings.append(&mut Self::find_ids(lhs));
                strings.append(&mut Self::find_ids(rhs));
            }
            Unary { uop, lhs } => {
                return Self::find_ids(lhs);
            }
            Term(t) => {
                if let Identifier(id) = t {
                    strings.push(id.clone());
                }
            }
        }

        strings
    }
    
    pub(crate) fn augment_grammar_with_starting_production(&mut self) {
        let dangling = self.find_dangling_ids();
        for dangle in dangling {
            self.decls.push( Decl {
                identifier: "II_INITIAL_PRODUCTION_II".to_string(),
                maps_to: Term(Identifier(dangle)),
            })
        }
    }
}

#[cfg(test)]
pub mod tests {
    use regex::Regex;
    use crate::lr_ast::{BinaryOperation, Decl, NonTerminal};
    use crate::lr_ast::BinaryOperation::{Concat, Or};
    use crate::lr_ast::NonTerminal::{Binary, Term};
    use crate::lr_ast::Terminal::{Empty, Identifier, StringLiteral};
    use crate::lr_augmenter::LrAugmenter;
    use crate::nimbus_lexer::TokenType::String;

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
    pub fn add_starting_production() {

        let mut generator = LrAugmenter::new(SELF_G).unwrap();
        generator.augment_grammar();
        generator.augment_grammar_with_starting_production();
        println!("add starting production: {:#?}", generator.decls);
    }

    #[test]
    pub fn find_dangling_ids() {
        let mut generator = LrAugmenter::new(SELF_G).unwrap();
        generator.augment_grammar();
        assert_eq!(vec!["decl"], generator.find_dangling_ids());
    }

    #[test]
    pub fn test_klein_plus_expansion_harder() {
        let g = r#"E -> "a"+ b"#;
        let mut generator = LrAugmenter::new(g).unwrap();
        generator.augment_grammar();
        assert_eq!(generator.decls, vec![
            Decl {
                identifier: "E".to_string(),
                maps_to: Binary {
                    lhs: Box::new(Term(Identifier("II_PLUS_EXPAND_2_II".to_string()))),
                    rhs: Box::new(Term(Identifier("b".to_string()))),
                    bop: BinaryOperation::Concat,
                },
            },
            Decl {
                identifier: "II_PLUS_EXPAND_2_II".to_string(),
                maps_to: Binary {
                    lhs: Box::new(Term(StringLiteral(Regex::new("a").unwrap()))),
                    rhs: Box::new(Binary {
                        lhs: Box::new(Term(StringLiteral(Regex::new("a").unwrap()))),
                        rhs: Box::new(Term(Identifier("II_PLUS_EXPAND_2_II".to_string()))),
                        bop: BinaryOperation::Concat,
                    }),
                    bop: BinaryOperation::Or,
                },
            }
        ])
    }

    #[test]
    pub fn test_paren_work() {
        let g = r#"E -> (a b)"#;
        let mut generator = LrAugmenter::new(g).unwrap();
        generator.augment_grammar();
        assert_eq!(generator.decls, vec![
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
        let mut generator = LrAugmenter::new(g).unwrap();
        generator.augment_grammar();
        assert_eq!(generator.decls, vec![
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
        let mut generator = LrAugmenter::new(g).unwrap();
        generator.augment_grammar();
        assert_eq!(generator.decls, vec![
            Decl {
                identifier: "E".to_string(),
                maps_to: Term(Identifier("II_PLUS_EXPAND_2_II".to_string())),
            },
            Decl {
                identifier: "II_PLUS_EXPAND_2_II".to_string(),
                maps_to: Binary {
                    lhs: Box::new(Term(StringLiteral(Regex::new("a").unwrap()))),
                    rhs: Box::new(Binary {
                        lhs: Box::new(Term(StringLiteral(Regex::new("a").unwrap()))),
                        rhs: Box::new(Term(Identifier("II_PLUS_EXPAND_2_II".to_string()))),
                        bop: BinaryOperation::Concat,
                    }),
                    bop: BinaryOperation::Or,
                },
            }
        ])
    }

    #[test]
    pub fn test_klein_star_expansion() {
        let g = r#"E -> "a"*"#;
        let mut generator = LrAugmenter::new(g).unwrap();
        generator.augment_grammar();
        assert_eq!(generator.decls, vec![
            Decl {
                identifier: "E".to_string(),
                maps_to: Binary {
                    lhs: Box::new(Term(Identifier("II_PLUS_EXPAND_2_II".to_string()))),
                    rhs: Box::new(Term(Empty)),
                    bop: BinaryOperation::Or,
                },
            },
            Decl {
                identifier: "II_PLUS_EXPAND_2_II".to_string(),
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
                                "II_PLUS_EXPAND_2_II".to_string(),
                            ),
                        )),
                        bop: Concat,
                    }),
                    bop: Or,
                },
            },
        ])
    }

    #[test]
    pub fn test_optional_expansion() {
        let g = r#"E -> "a"?"#;
        let mut generator = LrAugmenter::new(g).unwrap();
        generator.augment_grammar();
        assert_eq!(generator.decls, vec![
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