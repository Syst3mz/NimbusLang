use logos::Logos;

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

    #[regex(r#""([^\\"]|\\.)*""#)]
    StringLiteral,

    #[regex(r#"[ \t\n\r]+"#, logos::skip)]
    #[error]
    LexErr,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct Token {
    pub(crate) token_type: LRToken,
    pub(crate) lexeme: String
}

#[derive(Debug)]
pub(crate) struct LRLexer {
    pub(crate) lexed: Vec<Token<>>,
    pub(crate) idx: usize
}

impl<'source> LRLexer<> {
    pub fn new(input: &'source str) -> Self {
        let mut lexed = Vec::<Token>::new();
        let mut lexer = LRToken::lexer(input);
        while let Some(tok) = lexer.next() {
            lexed.push(Token {
                token_type: tok,
                lexeme: String::from(lexer.slice()),
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

impl<'source> Iterator for LRLexer  {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let out = match self.lexed.get(self.idx) {
            None => {None}
            Some(t) => {Some(t.clone())}
        };
        self.idx += 1;
        out
    }
}

impl<'source> ExactSizeIterator for LRLexer {
}

#[cfg(test)]
pub mod tests {
    use crate::lr_lexer::{LRLexer, LRToken, Token};

    #[test]
    fn check_string_literal() {
        let s = "\"a\"";

        let lex = LRLexer::new(s);
        for lexed in lex {
            assert_eq!(lexed, Token {
                token_type: LRToken::StringLiteral,
                lexeme: "\"a\"".to_string(),
            })
        }
    }

    #[test]
    fn check_string_literal_or() {
        let s = "\"|\"";

        let lex = LRLexer::new(s);
        for lexed in lex {
            assert_eq!(lexed, Token {
                token_type: LRToken::StringLiteral,
                lexeme: s.to_string(),
            })
        }
    }


}