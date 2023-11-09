use crate::source_info::SourceInfo;
use std::{collections::HashMap, rc::Rc};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Tok {
    pub kind: TokKind,
    pub source_info: SourceInfo,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TokKind {
    // keywords
    Let,
    While,
    Do,
    End,
    Return,
    Func,
    Struct,
    If,
    Then,
    Else,
    As,
    True,
    False,
    // punctuation
    Colon,
    ColonEq,
    ParLeft,
    ParRight,
    Arrow,
    Semi,
    AngleLeft,
    AngleRight,
    And,
    Dot,
    Comma,
    Plus,
    Minus,
    Star,
    Slash,
    At,
    Identifier(Identifier),
    IntLiteral(IntLiteral),
    // control
    Ignored,
    EndOfInput,
    Invalid,
}

impl TokKind {
    pub fn from_ident(str: String) -> TokKind {
        KEYWORD_TOKENS
            .with(|pair| pair.clone())
            .0
            .get(&str)
            .cloned()
            .unwrap_or(TokKind::Identifier(Identifier { value: str }))
    }

    fn to_keyword(&self) -> Option<String> {
        KEYWORD_TOKENS
            .with(|pair| pair.clone())
            .1
            .get(self)
            .map(|s| s.to_string())
    }
}

type KeywordTokens = Rc<(HashMap<String, TokKind>, HashMap<TokKind, String>)>;

fn keyword_tokens() -> KeywordTokens {
    let pairs = vec![
        (TokKind::Let, "let"),
        (TokKind::While, "while"),
        (TokKind::Func, "func"),
        (TokKind::Struct, "struct"),
        (TokKind::Do, "do"),
        (TokKind::Return, "return"),
        (TokKind::If, "if"),
        (TokKind::Then, "then"),
        (TokKind::Else, "else"),
        (TokKind::End, "end"),
        (TokKind::True, "true"),
        (TokKind::False, "false"),
    ];

    Rc::new((
        pairs
            .iter()
            .map(|(token, str)| (str.to_string(), token.clone()))
            .collect(),
        pairs
            .iter()
            .map(|(token, str)| (token.clone(), str.to_string()))
            .collect(),
    ))
}

thread_local! {
  static KEYWORD_TOKENS: KeywordTokens = keyword_tokens();
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Default)]
pub struct Identifier {
    pub value: String,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Default)]
pub struct IntLiteral {
    pub value: u128,
}
