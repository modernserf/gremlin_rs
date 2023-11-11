use crate::source_info::SourceInfo;
use crate::token::{IntLiteral, Tok, TokKind};

pub struct Lexer {
    chars: Vec<char>,
    index: usize,
}

impl Lexer {
    pub fn lex(str: &str) -> Vec<Tok> {
        let mut out = vec![];
        let mut lexer = Lexer::new(str);
        loop {
            let start = lexer.index;
            let tok_kind = lexer.next();
            if tok_kind == TokKind::Ignored {
                continue;
            }
            if tok_kind == TokKind::EndOfInput {
                out.push(Tok {
                    kind: TokKind::EndOfInput,
                    source_info: SourceInfo { start, length: 0 },
                });
                return out;
            }

            let length = lexer.index - start;
            out.push(Tok {
                kind: tok_kind,
                source_info: SourceInfo { start, length },
            });
        }
    }
    fn new(str: &str) -> Self {
        Lexer {
            chars: str.chars().collect(),
            index: 0,
        }
    }
    fn peek(&self) -> char {
        if self.index >= self.chars.len() {
            return '\0';
        }
        self.chars[self.index]
    }
    fn advance(&mut self) {
        if self.index == self.chars.len() {
            panic!("unexpected end of input")
        }
        self.index += 1;
    }
    fn adv_next(&mut self, tok: TokKind) -> TokKind {
        self.advance();
        tok
    }
    fn next(&mut self) -> TokKind {
        match self.peek() {
            '\0' => TokKind::EndOfInput,
            '#' => self.comment(),
            ' ' | '\t' | '\n' => self.whitespace(),
            '0'..='9' => self.number(),
            'a'..='z' | 'A'..='Z' => self.identifier_or_keyword(),
            '+' => self.adv_next(TokKind::Plus),
            '*' => self.adv_next(TokKind::Star),
            '(' => self.adv_next(TokKind::ParLeft),
            ')' => self.adv_next(TokKind::ParRight),
            '{' => self.adv_next(TokKind::CurlyLeft),
            '}' => self.adv_next(TokKind::CurlyRight),
            '&' => self.adv_next(TokKind::Ampersand),
            '@' => self.adv_next(TokKind::At),
            '.' => self.adv_next(TokKind::Dot),
            ';' => self.adv_next(TokKind::Semicolon),
            ':' => {
                self.advance();
                match self.peek() {
                    '=' => self.adv_next(TokKind::ColonEq),
                    _ => TokKind::Colon,
                }
            }
            _ => self.adv_next(TokKind::Invalid),
        }
    }
    fn comment(&mut self) -> TokKind {
        loop {
            match self.peek() {
                '\n' | '\0' => return TokKind::Ignored,
                _ => {
                    self.advance();
                }
            }
        }
    }
    fn whitespace(&mut self) -> TokKind {
        loop {
            if self.peek().is_whitespace() {
                self.advance()
            } else {
                return TokKind::Ignored;
            }
        }
    }
    fn number(&mut self) -> TokKind {
        let mut sum = 0;
        while let Some(digit) = self.peek().to_digit(10) {
            self.advance();
            sum = sum * 10 + (digit as u128)
        }
        TokKind::IntLiteral(IntLiteral { value: sum })
    }
    fn identifier_or_keyword(&mut self) -> TokKind {
        let mut str = String::new();
        loop {
            let ch = self.peek();
            if ch.is_alphanumeric() || ch == '_' {
                self.advance();
                str.push(ch);
            } else {
                return TokKind::from_ident(str);
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn assert_lex_eq(str: &str, tokens: Vec<Tok>) {
        let mut res = Lexer::lex(str);
        res.pop().expect("end of input");
        assert_eq!(res, tokens);
    }

    #[test]
    fn empty_program() {
        assert_lex_eq("", vec![])
    }

    #[test]
    fn token_with_source_info() {
        assert_lex_eq(
            "  123  ",
            vec![Tok {
                kind: TokKind::IntLiteral(IntLiteral { value: 123 }),
                source_info: SourceInfo {
                    start: 2,
                    length: 3,
                },
            }],
        )
    }

    #[test]
    fn invalid_token() {
        assert_lex_eq(
            " \x01 ",
            vec![Tok {
                kind: TokKind::Invalid,
                source_info: SourceInfo {
                    start: 1,
                    length: 1,
                },
            }],
        )
    }
}
