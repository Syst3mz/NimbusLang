use std::collections::HashMap;
use std::hash::Hash;
use crate::lr_ast::{BinaryOperation, Decl, NonTerminal, UnaryOperation};
use crate::lr_ast::NonTerminal::{Binary, Term};
use crate::lr_generator::GeneratorErr::ParserErr;
use crate::lr_parser;
use crate::lr_parser::{LRParser};

#[derive(Debug)]
pub(crate) enum DotPosition {
    Before,
    Inside,
    After
}

#[derive(Debug)]
pub(crate) struct LRItem {
    rule: NonTerminal,
    dot: DotPosition
}

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
pub(crate) struct LRGenerator {
    parsed: HashMap<String, Decl>
}

impl LRGenerator {
    pub fn new(text: &str) -> Result<Self, GeneratorErr> {
        Ok(Self { parsed: LRParser::new(text)
            .parse()?
            .iter()
            .map(|x| (x.identifier.to_owned(), x.clone()))
            .collect()})
    }
    pub(crate) fn modify_lr_grammar(&mut self) {
        for (key, value) in self.parsed {
            self.parsed[key] = Self::remove_unary(value)
        }
    }
    fn remove_unary(decl: Decl) -> Decl {
        Decl {
            identifier: decl.identifier,
            maps_to: Self::remove_unary_non_terminal(decl.maps_to),
        }
    }


    pub(crate) fn generate_lr_items(&self) -> HashMap<String, Vec<LRItem>> {
        let mut map: HashMap<String, Vec<LRItem>> = HashMap::new();
        for (key, value) in &self.parsed {
            map.insert(key.clone(), Self::get_decl_items(value));
        }
        map
    }
    fn remove_unary_non_terminal(nt: NonTerminal) -> NonTerminal {
        match nt {
            NonTerminal::Unary { uop, lhs } => {
                match uop {
                    UnaryOperation::Plus => {
                        Binary {
                            lhs,
                            rhs: Box::new(Binary {
                                lhs.co,
                                rhs: Box::new(lhs),
                                bop: BinaryOperation::Concat,
                            }),
                            bop: BinaryOperation::Or,
                        }
                    }
                    UnaryOperation::Star => {}
                    UnaryOperation::Optional => {}
                }
            }
            _ => nt
        }
    }

    fn get_decl_items(decl: &Decl) -> Vec<LRItem> {
        Self::get_non_terminal_items(&decl.maps_to)
    }
    fn get_non_terminal_items(nt: &NonTerminal) -> Vec<LRItem> {
        match nt {
            NonTerminal::Unary {uop, lhs} => {
                match uop {
                    UnaryOperation::Plus => {}
                    UnaryOperation::Star => {}
                    UnaryOperation::Optional => {}
                }

                vec![]
            },

            NonTerminal::Term(t) => {vec![
                LRItem {
                rule: Term(t.clone()),
                dot: DotPosition::Before,
            },
                LRItem {
                    rule: Term(t.clone()),
                    dot: DotPosition::After,
                },
            ]}
            _ => panic!("Unrecognized nonsense")
        }
    }
}

#[cfg(test)]
pub mod tests {
    use crate::lr_generator::LRGenerator;

    #[test]
    pub fn demo_items_expansion() {
        let g = r#"E -> "a""#;
        let generator = LRGenerator::new(g).unwrap();
        println!("demo items expansion: {:?}", generator.generate_lr_items());
    }

    #[test]
    pub fn make_sure_nothing_broke_between_phases() {
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
        println!("working as intended: {:?}", LRGenerator::new(g).unwrap());
    }
}