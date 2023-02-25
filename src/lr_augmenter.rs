use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::io::empty;
use crate::lr_ast::{BinaryOperation, Decl, NonTerminal, Terminal, UnaryOperation};
use crate::lr_ast::NonTerminal::{Binary, Term, Unary};
use crate::lr_ast::Terminal::{Empty, Identifier};
use crate::lr_augmenter::GeneratorErr::ParserErr;
use crate::lr_parser;
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
    pub backing_vec: Vec<Decl>
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

        Ok(Self { backing_vec } )
    }

    pub(crate) fn augment_grammar(&mut self) {
        let mut idx = 0;
        while idx < self.backing_vec.len() {
            self.backing_vec[idx].maps_to = self.augment_production(self.backing_vec[idx].maps_to.clone());
            idx += 1;
        }

        self.augment_grammar_with_starting_production();
    }

    fn augment_or(&mut self, lhs: NonTerminal, rhs: NonTerminal) -> NonTerminal {
        let lhs = self.augment_production(lhs);
        let rhs = self.augment_production(rhs);
        let id = format!("II_OR_EXPAND_{}_II", self.backing_vec.len() + 1);
        self.backing_vec.push(Decl {
            identifier: id.clone(),
            maps_to: lhs
        });
        self.backing_vec.push(Decl {
            identifier: id.clone(),
            maps_to: rhs
        });
        Term(Identifier(id))
    }

    fn augment_production(&mut self, nt: NonTerminal) -> NonTerminal {
        match nt {
            Binary { lhs, rhs, bop } => {
                match bop {
                    BinaryOperation::Or => {
                        self.augment_or(*lhs, *rhs)
                    }
                    BinaryOperation::Concat => {
                        Binary {
                            lhs: Box::new(self.augment_production(*lhs)) ,
                            rhs: Box::new(self.augment_production(*rhs)), bop}
                    }
                }
            },
            Unary { uop, lhs } => self.augment_unary(uop, lhs),
            Term(_) => nt
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
        let id = format!("II_PLUS_EXPAND_{}_II", self.backing_vec.len()+1);
        self.backing_vec.push(Decl {
            identifier: id.clone(),
            maps_to: Binary {
                lhs: lhs.clone(),
                rhs: Box::new(Binary {
                    lhs: lhs.clone(),
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
        for decl in &self.backing_vec {
            for term in Self::find_ids(&decl.maps_to) {
                ids_on_right.insert(term);
            }
        }

        let mut ids_unused: Vec<String> = vec![];
        for decl in &self.backing_vec {
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
            self.backing_vec.push( Decl {
                identifier: "II_INITIAL_PRODUCTION_II".to_string(),
                maps_to: Term(Identifier(dangle)),
            })
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct IdentifierTree {
    id: String,
    children: Vec<IdentifierTree>
}

#[cfg(test)]
pub mod tests {
    use crate::lr_augmenter::LrAugmenter;

    #[test]
    pub fn simple_test() {
        let g = r#"A -> b?"#;
        let mut generator = LrAugmenter::new(g).unwrap();
        generator.augment_grammar();
        println!("simple_test: {:#?}", generator.backing_vec);
    }

    #[test]
    pub fn add_starting_production() {
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
        println!("add starting production: {:#?}", generator.backing_vec);
    }

    #[test]
    pub fn find_dangling_ids() {
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
        println!("find_dangling_ids: {:#?}", generator.find_dangling_ids());
    }

    #[test]
    pub fn try_parse_self() {
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
        println!("try parse self: {:#?}", generator.backing_vec);
    }

    #[test]
    pub fn test_klein_plus_expansion_harder() {
        let g = r#"E -> "a"+ b"#;
        let mut generator = LrAugmenter::new(g).unwrap();
        generator.augment_grammar();
        println!("klein plus expansion harder: {:#?}", generator.backing_vec);
    }

    #[test]
    pub fn test_klein_plus_expansion() {
        let g = r#"E -> "a"+"#;
        let mut generator = LrAugmenter::new(g).unwrap();
        generator.augment_grammar();
        println!("klein plus expansion: {:#?}", generator.backing_vec);
    }

    #[test]
    pub fn test_klein_star_expansion() {
        let g = r#"E -> "a"*"#;
        let mut generator = LrAugmenter::new(g).unwrap();
        generator.augment_grammar();
        println!("klein plus expansion: {:#?}", generator.backing_vec);
    }

    #[test]
    pub fn test_optional_expansion() {
        let g = r#"E -> "a"?"#;
        let mut generator = LrAugmenter::new(g).unwrap();
        generator.augment_grammar();
        println!("optional : {:#?}", generator.backing_vec);
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
        println!("working as intended: {:?}", LrAugmenter::new(g).unwrap());
    }
}