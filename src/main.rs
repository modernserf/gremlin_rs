use std::{collections::HashMap, rc::Rc};

/* Gremlin grammar
Stmt =
    | "let" Binding (":" TyExpr)? ":=" Expr
    | Expr ":=" Expr
    | "while" Expr "do" Block "end"
    | "return" Expr?
    | "func" identifier "(" (Binding ":" TyExpr) ** "," ")" ("->" TyExpr)? "do" Block "end"
    | "struct" identifier "do" (identifier ":" TyExpr) ** "," "end"
    | Expr
Block = Stmt ** ";"
Binding = identifier
TyExpr =
    | identifier "<" TyExpr ** "," ">"
    | identifier
    | "&" TyExpr
Expr =
    | "(" Expr ")"
    | "if" Expr "then" Block ("else" "if" Expr "then" Block)* ("else" Block)? "end"
    | Expr "(" Expr ** "," ")"
    | Expr "." identifier
    | Expr Op Expr
    | Op Expr
    | Expr "as" TyExpr
    | identifier
    | number
    | string
    | "true"
    | "false"
Op = "+" | "-" | "*" | "/" | "&" | "@"
Comment = "#" ... eol
*/

#[derive(Clone, Debug, PartialEq, Eq)]
struct Tok {
    kind: TokKind,
    // TODO: token info
}

impl Tok {
    fn new(kind: TokKind) -> Self {
        Self { kind }
    }
    fn end_of_input() -> Self {
        Self {
            kind: TokKind::EndOfInput,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum TokKind {
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
    EndOfInput,
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
struct Identifier {
    value: String,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Default)]
struct IntLiteral {
    value: u128,
}

struct Lexer {
    chars: Vec<char>,
    index: usize,
}

impl Lexer {
    pub fn lex(str: &str) -> Vec<Tok> {
        let mut out = vec![];
        let mut lexer = Lexer::new(str);
        loop {
            let tok_kind = lexer.next();
            if tok_kind == TokKind::EndOfInput {
                return out;
            }
            out.push(Tok::new(tok_kind));
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
    fn next(&mut self) -> TokKind {
        match self.peek() {
            '\0' => TokKind::EndOfInput,
            '#' => self.comment(),
            ' ' | '\t' | '\n' => self.whitespace(),
            '0'..='9' => self.number(),
            'a'..='z' | 'A'..='Z' => self.identifier_or_keyword(),
            '+' => {
                self.advance();
                TokKind::Plus
            }
            _ => panic!("unknown char"),
        }
    }
    fn comment(&mut self) -> TokKind {
        loop {
            match self.peek() {
                '\n' | '\0' => return self.next(),
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
                return self.next();
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
            if ch.is_alphanumeric() || ch == '\'' || ch == '_' {
                self.advance();
                str.push(ch);
            } else {
                return TokKind::from_ident(str);
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Expr {
    kind: ExprKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum ExprKind {
    IntLiteral(IntLiteral),
    BinaryOp(Box<BinaryOp>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct BinaryOp {
    operator: OpKind,
    left: Expr,
    right: Expr,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum OpKind {
    Plus,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Parser {
    tokens: Vec<Tok>,
    index: usize,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct ParseError {
    kind: ParseErrKind,
}

impl ParseError {
    fn expected_token(tok: TokKind) -> Self {
        Self {
            kind: ParseErrKind::ExpectedToken(tok),
        }
    }
    fn expected(name: &str) -> Self {
        Self {
            kind: ParseErrKind::Expected(String::from(name)),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum ParseErrKind {
    Expected(String),
    ExpectedToken(TokKind),
}

type Parse<T> = Result<T, ParseError>;
pub type ParseOpt<T> = Result<Option<T>, ParseError>;

fn expect<T>(name: &str, value: ParseOpt<T>) -> Parse<T> {
    match value {
        Ok(Some(value)) => Ok(value),
        _ => Err(ParseError::expected(name)),
    }
}

impl Parser {
    pub fn parse_expr(tokens: Vec<Tok>) -> Parse<Expr> {
        let mut parser = Self::new(tokens);
        expect("expr", parser.expr())
    }

    fn new(tokens: Vec<Tok>) -> Self {
        Self { tokens, index: 0 }
    }
    fn peek(&self) -> TokKind {
        self.tokens
            .get(self.index)
            .map(|t| t.kind.clone())
            .unwrap_or(TokKind::EndOfInput)
    }

    fn advance(&mut self) {
        self.index += 1
    }

    fn expect_token(&mut self, tok: TokKind) -> Parse<()> {
        if self.peek() == tok {
            self.advance();
            Ok(())
        } else {
            Err(ParseError::expected_token(tok))
        }
    }

    fn expr(&mut self) -> ParseOpt<Expr> {
        if let Some(mut left) = self.base_expr()? {
            loop {
                if let Some(operator) = self.binary_op()? {
                    let right = expect("expr", self.base_expr())?;
                    left = Expr {
                        kind: ExprKind::BinaryOp(Box::new(BinaryOp {
                            operator,
                            left,
                            right,
                        })),
                    };
                } else {
                    return Ok(Some(left));
                }
            }
        }
        Ok(None)
    }

    fn binary_op(&mut self) -> ParseOpt<OpKind> {
        match self.peek() {
            TokKind::Plus => {
                self.advance();
                Ok(Some(OpKind::Plus))
            }
            _ => Ok(None),
        }
    }

    fn base_expr(&mut self) -> ParseOpt<Expr> {
        match self.peek() {
            TokKind::IntLiteral(payload) => {
                self.advance();
                Ok(Some(Expr {
                    kind: ExprKind::IntLiteral(payload),
                }))
            }
            _ => Ok(None),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Interpreter {}

impl Interpreter {
    fn new() -> Self {
        Self {}
    }
    fn eval_expr(&mut self, expr: Expr) -> Result<u128, ()> {
        match expr.kind {
            ExprKind::IntLiteral(payload) => Ok(payload.value),
            ExprKind::BinaryOp(payload) => {
                let left = self.eval_expr(payload.left)?;
                let right = self.eval_expr(payload.right)?;
                match payload.operator {
                    OpKind::Plus => Ok(left + right),
                }
            }
            _ => unimplemented!(),
        }
    }
}

fn main() {
    println!("Hello, world!");
    Interpreter::new()
        .eval_expr(Parser::parse_expr(Lexer::lex("123")).unwrap())
        .unwrap();
}

#[cfg(test)]
mod test {
    use super::*;

    fn assert_expr_eq(str: &str, expected: u128) {
        let result = Interpreter::new()
            .eval_expr(Parser::parse_expr(Lexer::lex(str)).expect("expr"))
            .expect("value");
        assert_eq!(result, expected);
    }

    #[test]
    fn empty_program() {
        assert_eq!(Lexer::lex(""), vec![])
    }

    #[test]
    fn integers() {
        assert_expr_eq("123", 123);
    }

    #[test]
    fn ignore_whitespace() {
        assert_expr_eq("       123     ", 123);
    }

    #[test]
    fn ignore_comments() {
        assert_expr_eq(
            " 
        
        # before

        123 # a comment
        
        ",
            123,
        );
    }

    #[test]
    fn addition() {
        assert_expr_eq("1 + 2 + 3", 6);
    }
}
