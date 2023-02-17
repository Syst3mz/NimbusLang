use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::io::empty;
use crate::lr_ast::{BinaryOperation, Decl, NonTerminal, Terminal, UnaryOperation};
use crate::lr_ast::NonTerminal::{Binary, Term, Unary};
use crate::lr_ast::Terminal::{Empty, Identifier};
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
    parsed: HashMap<String, usize>,
    backing_vec: Vec<Decl>
}

impl LRGenerator {
    pub fn new(text: &str) -> Result<Self, GeneratorErr> {
        let mappings:Vec<(String, Decl)> = LRParser::new(text)
            .parse()?
            .iter()
            .map(|x| (x.identifier.to_owned(), x.clone()))
            .collect();
        let mut parsed = HashMap::new();
        let mut backing_vec = Vec::new();

        for (name, decl) in mappings {
            backing_vec.push(decl);
            parsed.insert(name, backing_vec.len() - 1);
        }

        Ok(Self { parsed, backing_vec} )
    }

    fn append(&mut self, name: String, decl: Decl) {
        self.backing_vec.push(decl);
        self.parsed.insert(name, self.backing_vec.len() - 1);
    }



    pub(crate) fn augment_grammar(&mut self) {
        for idx in 0..self.backing_vec.len() {
            self.backing_vec[idx].maps_to = self.augment_production(self.backing_vec[idx].maps_to.clone())
        }

        self.find_dangling_ids();
    }

    fn augment_production(&mut self, nt: NonTerminal) -> NonTerminal {
        match nt {
            Binary { lhs, rhs, bop } => {
                Binary {
                    lhs: Box::new(self.augment_production(*lhs)),
                    rhs: Box::new(self.augment_production(*rhs)),
                    bop,
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
        self.append(id.clone(), Decl {
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
        let nt = if dangling.len() == 1 {
            Term(Identifier(dangling[0].clone()))
        } else if dangling.len() >= 2 {
            let mut temp = Binary {
                lhs: Box::new(Term(Identifier(dangling[0].clone()))),
                rhs: Box::new(Term(Identifier(dangling[1].clone()))),
                bop: BinaryOperation::Or,
            };

            let mut idx = 1;
            while idx < dangling.len() {
                temp = Binary {
                    lhs: Box::new(temp),
                    rhs: Box::new(Term(Identifier(dangling[idx].clone()))),
                    bop: BinaryOperation::Or,
                };
                idx+= 1;
            }

            temp
        } else { panic!() };

        self.append("II_INITIAL_PRODUCTION_II".to_string(), Decl {
            identifier: "II_INITIAL_PRODUCTION_II".to_string(),
            maps_to: nt,
        })
    }
}

#[derive(Debug, Clone)]
pub(crate) struct IdentifierTree {
    id: String,
    children: Vec<IdentifierTree>
}

#[cfg(test)]
pub mod tests {
    use crate::lr_generator::LRGenerator;

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
        let mut generator = LRGenerator::new(g).unwrap();
        generator.augment_grammar();
        generator.augment_grammar_with_starting_production();
        println!("add starting production: {:#?}\n{:#?}", generator.parsed, generator.backing_vec);
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
        let mut generator = LRGenerator::new(g).unwrap();
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
        let mut generator = LRGenerator::new(g).unwrap();
        generator.augment_grammar();
        println!("try parse self: {:#?}\n{:#?}", generator.parsed, generator.backing_vec);
    }

    #[test]
    pub fn test_klein_plus_expansion_harder() {
        let g = r#"E -> "a"+ b"#;
        let mut generator = LRGenerator::new(g).unwrap();
        generator.augment_grammar();
        println!("klein plus expansion harder: {:#?}\n{:#?}", generator.parsed, generator.backing_vec);
    }

    #[test]
    pub fn test_klein_plus_expansion() {
        let g = r#"E -> "a"+"#;
        let mut generator = LRGenerator::new(g).unwrap();
        generator.augment_grammar();
        println!("klein plus expansion: {:#?}\n{:#?}", generator.parsed, generator.backing_vec);
    }

    #[test]
    pub fn test_klein_star_expansion() {
        let g = r#"E -> "a"*"#;
        let mut generator = LRGenerator::new(g).unwrap();
        generator.augment_grammar();
        println!("klein plus expansion: {:#?}\n{:#?}", generator.parsed, generator.backing_vec);
    }

    #[test]
    pub fn test_optional_expansion() {
        let g = r#"E -> "a"?"#;
        let mut generator = LRGenerator::new(g).unwrap();
        generator.augment_grammar();
        println!("optional : {:#?}", generator.parsed);
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