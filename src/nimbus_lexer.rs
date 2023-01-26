use std::fmt::{Display, Formatter};
use logos::{Lexer, Logos, Span};
use strum::Display;

use crate::nimbus_lexer::TokenType::LexErr;

#[derive(Copy, Clone, Debug, PartialEq, Logos, Display)]
pub enum TokenType {
    #[token("?")]
    QuestionMark,

    #[token("<")]
    LAngleBracket,

    #[token(">")]
    RAngleBracket,

    #[token("[")]
    LBracket,

    #[token("]")]
    RBracket,

    #[token(":")]
    Colon,

    #[token("fn")]
    Fn,

    #[token(".")]
    Dot,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("->")]
    Arrow,

    #[token("abstract")]
    Abstract,

    #[regex("(public)|(pub)")]
    Public,
    #[regex("(private)|(priv)")]
    Private,
    #[regex("(protected)|(prot)")]
    Protected,

    #[token("super")]
    Super,
    #[token("this")]
    This,

    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("none")]
    None,

    #[token("!")]
    Bang,
    #[token("-")]
    Tack,
    #[token("/")]
    Slash,
    #[token("*")]
    Star,
    #[token("+")]
    Plus,

    #[token(">=")]
    Geq,
    #[token("<=")]
    Leq,
    #[token("!=")]
    BangEquals,
    #[token("==")]
    EqualsEquals,
    #[token("||")]
    PipePipe,
    #[token("&&")]
    AndAnd,

    #[token("=")]
    Equals,

    #[token("if")]
    If,
    #[token("else")]
    Else,

    #[token("{")]
    LCurlyBrace,
    #[token("}")]
    RCurlyBrace,

    #[token("while")]
    While,

    #[token("return")]
    Return,

    #[token("for")]
    For,

    #[token("in")]
    In,

    #[token(";")]
    Semicolon,

    #[token("enum")]
    Enum,

    #[token("use")]
    Use,

    #[token("as")]
    As,

    #[token("class")]
    Class,
    #[token(",")]
    Comma,

    #[regex("[a-zA-z_]([a-zA-z_0-9])*")]
    Identifier,
    #[regex("[0-9]+")]
    Integer,
    #[regex("[0-9]+.[0-9]+")]
    Decimal,

    #[regex(r#""(.|\\n)*""#)]
    String,
    #[regex("'(.|\\n)'")]
    Char,

    #[regex(r"[ \t\n\f\r]+", logos::skip)]
    #[error]
    LexErr
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub tok_type: TokenType,
    pub location: Location,
    pub lexeme: String
}

#[derive(Debug, Clone, PartialEq)]
pub struct Location {
    span: Span,
}

#[derive(Debug, Clone)]
pub struct NimbusLexer<'source> {
    internal_lexer: Lexer<'source, TokenType>,
}

impl<'source> NimbusLexer<'source> {
    pub fn new(to_lex: &'source str) -> Self {
        Self {
            internal_lexer: TokenType::lexer(to_lex),
        }
    }

    pub fn to_vec(mut self) -> Vec<Token> {
        let mut ret:Vec<Token> = Vec::new();
        while let Some(tok) = self.next() {
            if tok.tok_type == LexErr {
                let (err, fine) = self.collect_adjacent_errs(tok);
                ret.push(err);
                ret.push(fine);
            }
            else {
                ret.push(tok)
            }
        }
        ret
    }

    fn collect_adjacent_errs(&mut self, trigger: Token) -> (Token, Token) {
        let mut err_vec:Vec<Token> = vec![trigger.clone()];
        while let Some(err_tok) = self.next() {
            err_vec.push(err_tok.clone());
            if err_tok.tok_type != LexErr{
                break
            }
        }
        let mut lexeme = trigger.lexeme;
        let valid = err_vec.pop().unwrap();
        for err_tok in err_vec {
            lexeme.push_str(err_tok.lexeme.as_str())
        }
        (Token {
            tok_type: LexErr,
            location: trigger.location,
            lexeme,
        }, valid)
    }

    fn get_location(&self, span: &Span) -> (usize, usize) {
        let mut row = 1;
        let mut col = 1;
        for chr in self.internal_lexer.source()[0..span.start].chars() {
            if chr == '\n' {
                row += 1;
                col = 1;
            }
            else if chr != '\r' { col += 1; }
        }

        (row, col)
    }
}

impl Iterator for NimbusLexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.internal_lexer.next() {
            None => None,
            Some(token) => {
                let s = self.internal_lexer.slice();

                Some(Token {
                    tok_type: token,
                    location: Location {
                        span: self.internal_lexer.span(),
                    },
                    lexeme: s.to_string(),
                })
            }
        }
    }
}

#[cfg(test)]
pub mod tests {
}
