use std::str::FromStr;
use regex::{Error, Regex};
use crate::lr_ast::{BinaryOperation, Decl, NonTerminal, Terminal, UnaryOperation};
use crate::lr_ast::NonTerminal::{Binary, Term, Unary};
use crate::lr_lexer::{LRLexer, LRToken, Token};
use crate::lr_lexer::LRToken::{Arrow, Identifier, LParen, RParen, StringLiteral};
use crate::lr_parser::ParserErr::{EOI, InvalidRegex, UnexpectedToken};

#[derive(Debug)]
pub(crate) enum ParserErr {
    EOI,
    UnexpectedToken(LRToken, Vec<LRToken>),
    InvalidRegex(Error)
}

pub(crate) struct LRParser {
    lexed: LRLexer,
}

impl<'source> LRParser {
    pub(crate) fn new(source: &'source str) -> Self {
        Self { lexed: LRLexer::new(source) }
    }

    pub(crate) fn parse(&mut self) -> Result<Vec<Decl>, ParserErr> {
        let mut ret = Vec::<Decl>::new();
        while let Some(_id) = self.lexed.peek(){
            ret.push(self.parse_decl()?);
        }
        Ok(ret)
    }

    fn match_tok(&mut self, tok:LRToken) -> Result<Token, ParserErr> {
        return if let Some(token) = self.lexed.current() {
            if token.token_type == tok {
                Ok(token)
            } else {
                Err(UnexpectedToken(token.token_type, vec![Arrow]))
            }
        } else {
            Err(EOI)
        }
    }

    fn parse_decl(&mut self) -> Result<Decl, ParserErr> {
        if let Some(identifier) = self.lexed.current() {
            if identifier.token_type != Identifier {
                return Err(UnexpectedToken(identifier.token_type, vec![Identifier]))
            }

            self.lexed.next();
            let name = identifier.lexeme;


            self.match_tok(Arrow)?;
            self.lexed.next();

            let non_terminal = self.parse_non_terminal();

            return Ok(Decl {
                identifier: name.to_string(),
                maps_to: non_terminal?,
            })
        }
        Err(EOI)
    }


    fn parse_non_terminal(&mut self) -> Result<NonTerminal, ParserErr> {
        let mut lhs = self.parse_unary()?;

        while let Some(next_tok) = self.lexed.next() {
            match next_tok.token_type {
                Identifier => {
                    if let Some(mb_arrow) = self.lexed.current() {
                        if mb_arrow.token_type == Arrow {
                            self.lexed.rewind();
                            return Ok(lhs)
                        }
                    }

                    self.lexed.rewind();
                    lhs = Binary {
                        lhs: Box::new(lhs),
                        rhs: Box::new(self.parse_unary()?),
                        bop: BinaryOperation::Concat,
                    };
                },

                StringLiteral | LParen => {
                    self.lexed.rewind();
                    lhs = Binary {
                        lhs: Box::new(lhs),
                        rhs: Box::new(self.parse_unary()?),
                        bop: BinaryOperation::Concat,
                    };
                }
                LRToken::Or => {
                    lhs = Binary {
                        lhs: Box::new(lhs),
                        rhs: Box::new(self.parse_unary()?),
                        bop: BinaryOperation::Or,
                    }
                }
                _ => {
                    self.lexed.rewind();
                    return Ok(lhs)
                }
            };
        }

        return Ok(lhs)
    }

    fn parse_unary(&mut self) -> Result<NonTerminal, ParserErr> {
        let mut lhs = self.parse_paren()?;

        if let Some(token) = self.lexed.current(){
            lhs = match token.token_type {
                LRToken::Plus => {
                    self.lexed.next();
                    Unary {
                        uop: UnaryOperation::Plus,
                        lhs: Box::new(lhs),
                    }
                },
                LRToken::Star => {
                    self.lexed.next();
                    Unary {
                        uop: UnaryOperation::Star,
                        lhs: Box::new(lhs),
                    }
                },
                LRToken::QuestionMark => {
                    self.lexed.next();
                    Unary {
                        uop: UnaryOperation::Optional,
                        lhs: Box::new(lhs),
                    }
                }
                _ => {lhs}
            }
        }

        Ok(lhs)
    }

    // always advances
    fn parse_paren(&mut self) -> Result<NonTerminal, ParserErr> {
        if let Ok(_paren) = self.match_tok(LParen) {
            self.lexed.next();

            let inner = self.parse_non_terminal();
            self.match_tok(RParen)?;
            self.lexed.next();

            inner
        }
        else {
            Ok(Term(self.parse_terminal()?))
        }
    }

    // always advances
    fn parse_terminal(&mut self) -> Result<Terminal, ParserErr> {
        return if let Some(tok) = self.lexed.next()  {
            match tok.token_type {
                StringLiteral => {
                    let regex = Regex::from_str(&tok.lexeme.as_str()[1..tok.lexeme.len() - 1]);
                    match regex {
                        Ok(reg) => { Ok(Terminal::StringLiteral(reg)) }
                        Err(e) => { Err(InvalidRegex(e)) }
                    }
                }
                Identifier => {
                    Ok(Terminal::Identifier(tok.lexeme.to_string()))
                }
                _ => { Err(UnexpectedToken(tok.token_type, vec![StringLiteral, Identifier])) }
            }
        } else {
            Err(EOI)
        }
    }
}

#[cfg(test)]
pub(crate) mod test {
    use regex::Regex;
    use crate::lr_ast::{BinaryOperation, Decl, UnaryOperation};
    use crate::lr_ast::NonTerminal::{Binary, Term, Unary};
    use crate::lr_ast::Terminal::{Identifier, StringLiteral};
    use crate::lr_lexer::{LRLexer, LRToken, Token};
    use crate::lr_parser::LRParser;


    #[test]
    fn check_parser_1_rule() {
        let g = r#"decl -> stmnt stmnt2"#;
        let r = LRParser::new(g).parse();
        assert_eq!(r.unwrap(), vec![
            Decl {
                identifier: "decl".to_string(),
                maps_to: Binary {
                    lhs: Box::new(Term(Identifier("stmnt".to_string()))),
                    rhs: Box::new(Term(Identifier("stmnt2".to_string()))),
                    bop: BinaryOperation::Concat,
                },
            }
        ])
    }

    #[test]
    fn check_parser_1_rule_many() {
        let g = r#"decl -> stmnt stmnt2 stmnt3 stmnt4"#;
        let r = LRParser::new(g).parse();
        assert_eq!(r.unwrap(), vec![
            Decl {
                identifier: "decl".to_string(),
                maps_to: Binary {
                    lhs: Box::new(Binary {
                        lhs: Box::new(Binary {
                            lhs: Box::new(Term(Identifier("stmnt".to_string()))),
                            rhs: Box::new(Term(Identifier("stmnt2".to_string()))),
                            bop: BinaryOperation::Concat,
                        }),
                        rhs: Box::new(Term(Identifier("stmnt3".to_string()))),
                        bop: BinaryOperation::Concat,
                    }),
                    rhs: Box::new(Term(Identifier("stmnt4".to_string()))),
                    bop: BinaryOperation::Concat,
                },
            }
        ])
    }

    #[test]
    fn check_parser_two_rules_each_singles() {
        let g = r#"decl -> stmnt
         decl2 -> stmnt2"#;
        let r = LRParser::new(g).parse();
        assert_eq!(r.unwrap(), vec![
            Decl {
                identifier: "decl".to_string(),
                maps_to: Term(Identifier("stmnt".to_string())),
            },
            Decl {
                identifier: "decl2".to_string(),
                maps_to: Term(Identifier("stmnt2".to_string())),
            }
        ])
    }

    #[test]
    fn check_parser_two_rules_multi() {
        let g = r#"decl -> stmnt stmnt2
         decl2 -> stmnt3"#;
        let r = LRParser::new(g).parse();
        assert_eq!(r.unwrap(), vec![
            Decl {
                identifier: "decl".to_string(),
                maps_to: Binary {
                    lhs: Box::new(Term(Identifier("stmnt".to_string()))),
                    rhs: Box::new( Term(Identifier("stmnt2".to_string()))),
                    bop: BinaryOperation::Concat,
                }
            },
            Decl {
                identifier: "decl2".to_string(),
                maps_to: Term(Identifier("stmnt3".to_string())),
            }
        ])
    }

    #[test]
    fn check_parser_paren() {
        let g = r#"decl -> (a)"#;
        let r = LRParser::new(g).parse();
        assert_eq!(r.unwrap(), vec![
            Decl {
                identifier: "decl".to_string(),
                maps_to: Term(Identifier("a".to_string())),
            }
        ])
    }

    #[test]
    fn check_parser_paren_after() {
        let g = r#"decl -> (a) b"#;
        let r = LRParser::new(g).parse();
        assert_eq!(r.unwrap(), vec![
            Decl {
                identifier: "decl".to_string(),
                maps_to: Binary {
                    lhs: Box::new(Term(Identifier("a".to_string()))),
                    rhs: Box::new(Term(Identifier("b".to_string()))),
                    bop: BinaryOperation::Concat,
                },
            }
        ])
    }

    #[test]
    fn check_parser_postfix_1() {
        let g = r#"decl -> a+"#;
        let r = LRParser::new(g).parse();
        assert_eq!(r.unwrap(), vec![
            Decl {
                identifier: "decl".to_string(),
                maps_to: Unary {
                    uop: UnaryOperation::Plus,
                    lhs: Box::new(Term(Identifier("a".to_string()))),
                },
            }
        ])
    }

    #[test]
    fn check_parser_postfix_2() {
        let g = r#"decl -> a b+"#;
        let r = LRParser::new(g).parse();
        assert_eq!(r.unwrap(), vec![
            Decl {
                identifier: "decl".to_string(),
                maps_to: Binary {
                    lhs: Box::new(Term(Identifier("a".to_string()))),
                    rhs: Box::new(Unary {
                        uop: UnaryOperation::Plus,
                        lhs: Box::new(Term(Identifier("b".to_string()))),
                    }),
                    bop: BinaryOperation::Concat,
                },
            }
        ])
    }

    #[test]
    fn check_or() {
        let g = r#"decl -> a | b"#;
        let r = LRParser::new(g).parse();
        assert_eq!(r.unwrap(), vec![
            Decl {
                identifier: "decl".to_string(),
                maps_to: Binary {
                    lhs: Box::new(Term(Identifier("a".to_string()))),
                    rhs: Box::new(Term(Identifier("b".to_string()))),
                    bop: BinaryOperation::Or,
                },
            }
        ])
    }

    #[test]
    fn check_infix_string_literal() {
        let g = r#"decl -> a "a" b"#;
        let r = LRParser::new(g).parse();
        assert_eq!(r.unwrap(), vec![
            Decl {
                identifier: "decl".to_string(),
                maps_to: Binary {
                    lhs: Box::new(Binary {
                        lhs: Box::new(Term(Identifier("a".to_string()))),
                        rhs: Box::new(Term(StringLiteral(Regex::new("a").unwrap()))),
                        bop: BinaryOperation::Concat,
                    }),
                    rhs: Box::new(Term(Identifier("b".to_string()))),
                    bop: BinaryOperation::Concat,
                },
            }
        ])
    }

    #[test]
    fn parse_self() {
        let g = r#"decl         -> IDENTIFIER "->" nonterminal
nonterminal  -> terminal
			  | nonterminal "\|" nonterminal
			  | nonterminal nonterminal
			  | nonterminal "\+"
			  | nonterminal "\*"
			  | nonterminal "\?"
			  | "\(" nonterminal "\)"
terminal     -> REGEX
			  | IDENTIFIER"#;
        let r = LRParser::new(g).parse();
        println!("parse self: {:#?}", r.unwrap());
    }
}