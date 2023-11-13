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
    True,
    False,
    And,
    Or,
    Not,
    As,
    Type,
    Struct,
    End,
    // punctuation
    Semicolon,
    Colon,
    ColonEq,
    Dot,
    ParLeft,
    ParRight,
    CurlyLeft,
    CurlyRight,
    Ampersand,
    Plus,
    Star,
    At,
    Identifier(Identifier),
    IntLiteral(IntLiteral),
    LongLiteral(IntLiteral),
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
}

type KeywordTokens = Rc<(HashMap<String, TokKind>, HashMap<TokKind, String>)>;

fn keyword_tokens() -> KeywordTokens {
    let pairs = vec![
        (TokKind::Let, "let"),
        (TokKind::True, "true"),
        (TokKind::False, "false"),
        (TokKind::And, "and"),
        (TokKind::Or, "or"),
        (TokKind::Not, "not"),
        (TokKind::As, "as"),
        (TokKind::Type, "type"),
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
