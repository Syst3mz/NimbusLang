use std::iter::Peekable;
use std::str::FromStr;
use logos::{Lexer, Logos};
use regex::{Error, Regex};
use crate::lr_generator::LRToken::{Arrow, Identifier, RParen, StringLiteral, LParen};
use crate::lr_generator::NonTerminal::{Binary, Term, Unary};
use crate::lr_generator::ParserErr::{EOI, InvalidRegex, UnexpectedToken};
use crate::nimbus_lexer::TokenType;

#[derive(Logos, Debug, PartialEq, Clone, Copy)]
pub enum LRToken {

    #[token("(")]
    LParen,
    #[token(")")]
    RParen,

    #[token("?")]
    QuestionMark,
    #[token("*")]
    Star,
    #[token("+")]
    Plus,

    #[token("|")]
    Or,

    #[token("->")]
    Arrow,

    #[regex("[a-zA-Z]([a-zA-Z0-9_])*")]
    Identifier,

    #[regex(r#""[^"|^(\\")]*""#)]
    StringLiteral,

    #[regex(r#"[ \t\n\r]+"#, logos::skip)]
    #[error]
    LexErr,
}

#[derive(Debug, Clone)]
struct Decl {
    identifier: String,
    maps_to: NonTerminal
}

#[derive(Debug, Clone)]
enum BinaryOperation {
    Or,
    Concat
}

#[derive(Debug, Clone)]
enum UnaryOperation {
    Plus,
    Star,
    Optional
}

#[derive(Debug, Clone)]
enum NonTerminal {
    Binary {
        lhs: Box<NonTerminal>,
        rhs: Box<NonTerminal>,
        bop: BinaryOperation
    },

    Unary {
        uop: UnaryOperation,
        lhs: Box<NonTerminal>
    },
    Term(Terminal)
}

#[derive(Debug, Clone)]
enum Terminal {
    Identifier(String),
    StringLiteral(Regex)
}



#[derive(Debug, PartialEq, Clone)]
pub(crate) struct Token<'source> {
    token_type: LRToken,
    lexeme: &'source str
}

pub(crate) struct LRLexer<'source> {
    pub(crate) lexed: Vec<Token<'source>>,
    idx: usize
}

impl<'source> LRLexer<'source> {
    pub fn new(input: &'source str) -> Self {
        let mut lexed = Vec::<Token>::new();
        let mut lexer = LRToken::lexer(input);
        while let Some(tok) = lexer.next() {
            lexed.push(Token {
                token_type: tok,
                lexeme: lexer.slice(),
            })
        }
        Self {
            lexed,
            idx: 0
        }
    }

    pub fn current(&self) -> Option<Token> {
        match self.lexed.get(self.idx) {
            None => None,
            Some(t) => Some(t.clone())
        }
    }

    pub fn peek(&self) -> Option<Token> {
        match self.lexed.get(self.idx+1) {
            None => None,
            Some(t) => Some(t.clone())
        }
    }

    pub fn peek_n(&self, n: usize) -> Option<Token> {
        match self.lexed.get(self.idx+n) {
            None => None,
            Some(t) => Some(t.clone())
        }
    }

    pub fn rewind(&mut self) -> Option<Token> {
        if self.idx < 1 {
            return None;
        }

        self.idx -= 1;
        return match self.lexed.get(self.idx) {
            None => None,
            Some(t) => Some(t.clone())
        }
    }
}

impl<'source> Iterator for LRLexer<'source>  {
    type Item = Token<'source>;

    fn next(&mut self) -> Option<Self::Item> {
        let out = match self.lexed.get(self.idx) {
            None => {None}
            Some(t) => {Some(t.clone())}
        };
        self.idx += 1;
        out
    }
}

impl<'source> ExactSizeIterator for LRLexer<'source> {
}

#[derive(Debug)]
pub enum ParserErr {
    EOI,
    UnexpectedToken(LRToken, Vec<LRToken>),
    InvalidRegex(Error)
}

struct LRParser<'source> {
    lexed: LRLexer<'source>,
}

impl<'source> LRParser<'source> {
    pub fn new(source: &'source str) -> Self {
        Self { lexed: LRLexer::new(source) }
    }

    pub fn parse(&mut self) -> Result<Vec<Decl>, ParserErr> {
        let mut ret = Vec::<Decl>::new();
        while let Some(_id) = self.lexed.peek(){
            ret.push(self.parse_decl()?);
        }
        Ok(ret)
    }
    fn parse_decl(&mut self) -> Result<Decl, ParserErr> {
        if let Some(identifier) = self.lexed.next() {
            if identifier.token_type != Identifier {
                return Err(UnexpectedToken(identifier.token_type, vec![Identifier]))
            }
            let name = identifier.lexeme;
            self.parse_tok(Arrow)?;
            let non_terminal = self.parse_non_terminal();

            return Ok(Decl {
                identifier: name.to_string(),
                maps_to: non_terminal?,
            })
        }
        Err(EOI)
    }
    fn parse_tok(&mut self, tok:LRToken) -> Result<Token, ParserErr> {
        return if let Some(token) = self.lexed.next() {
            if token.token_type == tok {
                Ok(token)
            } else {
                Err(UnexpectedToken(token.token_type, vec![Arrow]))
            }
        } else {
            Err(EOI)
        }
    }

    fn parse_non_terminal(&mut self) -> Result<NonTerminal, ParserErr> {
        let mut lhs = Term(self.parse_terminal()?);
        while let Some(next_tok) = self.lexed.next() {

            if next_tok.token_type == Arrow {
                self.lexed.rewind();
                self.lexed.rewind();
                return Ok(lhs)
            }

            let found = match next_tok.token_type {
                Identifier => {
                    self.lexed.rewind();
                    Term(self.parse_terminal()?)
                }

                LParen => {
                    let internal = self.parse_non_terminal()?;
                    self.parse_tok(RParen)?;
                    internal
                }
                _ => return Ok(lhs)
            };

            lhs = Binary {
                lhs: Box::new(lhs),
                rhs: Box::new(found),
                bop: BinaryOperation::Concat,
            };
        }

        return Ok(lhs)
    }

    fn parse_terminal(&mut self) -> Result<Terminal, ParserErr> {
        return if let Some(tok) = self.lexed.next() {
            match tok.token_type {
                StringLiteral => {
                    let regex = Regex::from_str(tok.lexeme);
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
pub mod test {
    use crate::lr_generator::{BinaryOperation, Decl, LRLexer, LRParser, Token};
    use crate::lr_generator::LRToken::StringLiteral;
    use crate::lr_generator::NonTerminal::{Binary};

    #[test]
    fn check_string_literal() {
        let s = "\"a\"";

        let lex = LRLexer::new(s);
        for lexed in lex {
            assert_eq!(lexed, Token {
                token_type: StringLiteral,
                lexeme: "\"a\"",
            })
        }
    }


    #[test]
    fn check_parser_1_rule() {
        let g = r#"decl -> stmnt stmnt2"#;
        let r = LRParser::new(g).parse();
        println!("{:?}", r.unwrap());
    }

    #[test]
    fn check_parser_1_rule_many() {
        let g = r#"decl -> stmnt stmnt2 stmnt3 stmnt4"#;
        let r = LRParser::new(g).parse();
        println!("{:?}", r.unwrap());
    }

    #[test]
    fn check_parser_two_rules_each_singles() {
        let g = r#"decl -> stmnt
         decl2 -> stmnt3"#;
        let r = LRParser::new(g).parse();
        println!("{:?}", r.unwrap());
    }

    #[test]
    fn check_parser_two_rules_multi() {
        let g = r#"decl -> stmnt stmnt2
         decl2 -> stmnt3"#;
        let r = LRParser::new(g).parse();
        println!("{:?}", r.unwrap());
    }

    #[test]
    fn check_parser_paren() {
        let g = r#"decl -> (a)"#;
        let r = LRParser::new(g).parse();
        println!("{:?}", r.unwrap());
    }
}
