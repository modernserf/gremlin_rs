use crate::*;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    Integer(Word),
    Identifier(String),
    TypeIdentifier(String),
    EndOfInput,
    True,
    False,
    Let,
    As,
    Type,
    Struct,
    Array,
    OneOf,
    Volatile,
    Colon,
    Semicolon,
    Comma,
    Dot,
    ColonEq,
    Plus,
    Minus,
    Star,
    // At,
    Ampersand,
    ParLeft,
    ParRight,
    CurlyLeft,
    CurlyRight,
    SqLeft,
    SqRight,
}

pub struct Lexer {
    chars: Vec<char>,
    index: usize,
    current_token: Compile<Token>,
}
impl Lexer {
    pub fn new(str: &str) -> Self {
        Self {
            chars: str.chars().collect(),
            index: 0,
            current_token: Ok(Token::EndOfInput),
        }
    }
    pub fn peek(&mut self) -> Compile<Token> {
        if self.index == 0 {
            self.current_token = self.next_token();
        }
        self.current_token.clone()
    }
    pub fn advance(&mut self) {
        self.current_token = self.next_token()
    }

    pub fn token(&mut self, token: Token) -> CompileOpt<()> {
        if self.peek()? == token {
            self.advance();
            return Ok(Some(()));
        }
        return Ok(None);
    }

    pub fn expect_token(&mut self, tok: Token) -> Compile<()> {
        self.token(tok.clone())?.ok_or(ExpectedToken(tok))
    }

    pub fn op(&mut self) -> CompileOpt<Op> {
        let op = match self.peek()? {
            Token::Plus => Op::Add,
            Token::Minus => Op::Sub,
            Token::Star => Op::Mul,
            _ => return Ok(None),
        };
        self.advance();
        Ok(Some(op))
    }

    pub fn ident_token(&mut self) -> CompileOpt<String> {
        match self.peek()? {
            Token::Identifier(s) => {
                self.advance();
                Ok(Some(s))
            }
            _ => Ok(None),
        }
    }

    pub fn type_ident_token(&mut self) -> CompileOpt<String> {
        match self.peek()? {
            Token::TypeIdentifier(s) => {
                self.advance();
                Ok(Some(s))
            }
            _ => Ok(None),
        }
    }
    pub fn int_token(&mut self) -> CompileOpt<Word> {
        match self.peek()? {
            Token::Integer(value) => {
                self.advance();
                Ok(Some(value))
            }
            _ => Ok(None),
        }
    }

    fn next_token(&mut self) -> Compile<Token> {
        match self.peek_char() {
            ' ' | '\n' => self.whitespace(),
            '#' => self.comment(),
            '\0' => Ok(Token::EndOfInput),
            '0'..='9' => self.number(),
            'l' => self.keyword("let", Token::Let),
            't' => {
                self.adv_char();
                match self.peek_char() {
                    'r' => self.keyword_idx("true", 1, Token::True),
                    _ => self.keyword_idx("type", 1, Token::Type),
                }
            }
            'f' => self.keyword("false", Token::False),
            'a' => {
                self.adv_char();
                match self.peek_char() {
                    'r' => self.keyword_idx("array", 1, Token::Array),
                    _ => self.keyword_idx("as", 1, Token::As),
                }
            }
            's' => self.keyword("struct", Token::Struct),
            'o' => self.keyword("oneof", Token::OneOf),
            'v' => self.keyword("volatile", Token::Volatile),
            ';' => self.punc(Token::Semicolon),
            ',' => self.punc(Token::Comma),
            '.' => self.punc(Token::Dot),
            '+' => self.punc(Token::Plus),
            '-' => self.punc(Token::Minus),
            '*' => self.punc(Token::Star),
            '&' => self.punc(Token::Ampersand),
            '(' => self.punc(Token::ParLeft),
            ')' => self.punc(Token::ParRight),
            '{' => self.punc(Token::CurlyLeft),
            '}' => self.punc(Token::CurlyRight),
            '[' => self.punc(Token::SqLeft),
            ']' => self.punc(Token::SqRight),
            ':' => {
                self.adv_char();
                match self.peek_char() {
                    '=' => self.punc(Token::ColonEq),
                    _ => Ok(Token::Colon),
                }
            }
            'a'..='z' => self.identifier("").map(Token::Identifier),
            'A'..='Z' => self.identifier("").map(Token::TypeIdentifier),
            _ => Err(UnexpectedChar),
        }
    }
    fn whitespace(&mut self) -> Compile<Token> {
        loop {
            if self.peek_char().is_whitespace() {
                self.adv_char();
            } else {
                return self.next_token();
            }
        }
    }
    fn comment(&mut self) -> Compile<Token> {
        loop {
            let next = self.peek_char();
            if next == '\n' || next == '\0' {
                return self.next_token();
            }
            self.adv_char();
        }
    }
    fn number(&mut self) -> Compile<Token> {
        let mut sum = 0;
        while let Some(digit) = self.peek_char().to_digit(10) {
            self.adv_char();
            // TODO: overflow check
            sum = sum * 10 + (digit as Word)
        }
        Ok(Token::Integer(sum))
    }
    fn keyword(&mut self, keyword: &str, token: Token) -> Compile<Token> {
        self.keyword_idx(keyword, 0, token)
    }
    fn keyword_idx(&mut self, keyword: &str, start_index: usize, token: Token) -> Compile<Token> {
        for (i, ch) in keyword.chars().skip(start_index).enumerate() {
            if self.peek_char() != ch {
                return self
                    .identifier(&keyword[0..(i + start_index)])
                    .map(Token::Identifier);
            }
            self.adv_char();
        }
        let ch = self.peek_char();
        if ch.is_alphanumeric() || ch == '_' {
            return self.identifier(keyword).map(Token::Identifier);
        }

        Ok(token)
    }
    fn identifier(&mut self, prefix: &str) -> Compile<String> {
        let mut out = String::from(prefix);
        loop {
            let ch = self.peek_char();
            if ch.is_alphanumeric() || ch == '_' {
                self.adv_char();
                out.push(ch);
            } else {
                return Ok(out);
            }
        }
    }
    fn punc(&mut self, token: Token) -> Compile<Token> {
        self.adv_char();
        Ok(token)
    }

    fn peek_char(&self) -> char {
        if self.index >= self.chars.len() {
            return '\0';
        }
        self.chars[self.index]
    }
    fn adv_char(&mut self) {
        if self.index == self.chars.len() {
            panic!("unexpected end of input")
        }
        self.index += 1;
    }
}
