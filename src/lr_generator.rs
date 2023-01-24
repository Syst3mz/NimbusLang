use logos::{Lexer, Logos};

#[derive(Logos, Debug)]
enum LRToken {

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

    #[token("&")]
    And,
    #[token("|")]
    Or,

    #[token("->")]
    Arrow,

    #[regex("[a-zA-Z]([a-zA-Z0-9_])*")]
    Identifier,

    #[regex(r#""(.|\\n)*""#)]
    StringLiteral,

    #[regex(r"[ \t\n\f\r]+", logos::skip)]
    #[error]
    LexErr
}

#[derive(Debug)]
struct Token<'source> {
    token: LRToken,
    lexeme: &'source str
}

struct LRLexer<'source> {
    internal_lexer: Lexer<'source, LRToken>
}

impl<'source> LRLexer<'source> {
    pub fn new(input: &'source str) -> Self {
        Self { internal_lexer:LRToken::lexer(input) }
    }
}

impl<'source> Iterator for LRLexer<'source> {
    type Item = Token<'source>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.internal_lexer.next() {
            None => {None}
            Some(real) => {
                Some(Token { token: real, lexeme: self.internal_lexer.slice() })
            }
        }
    }
}

#[cfg(test)]
#[test]
fn check() {
    let s = r#""a""#;
    let lex = LRLexer::new(s);
    for lexed in lex {
        println!("{:?}", lexed)
    }
    assert!(true)
}