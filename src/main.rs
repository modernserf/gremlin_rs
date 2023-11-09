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

mod source_info {
    #[derive(Copy, Clone, Debug, PartialEq, Eq)]
    pub struct SourceInfo {
        pub start: usize,
        pub length: usize,
    }

    impl SourceInfo {
        pub fn span(&self, other: SourceInfo) -> SourceInfo {
            let end = other.start + other.length;
            SourceInfo {
                start: self.start,
                length: end - self.start,
            }
        }
    }

    #[cfg(test)]
    mod test {
        use super::*;

        #[test]
        fn span() {
            let left = SourceInfo {
                start: 0,
                length: 1,
            };
            let right = SourceInfo {
                start: 10,
                length: 2,
            };
            assert_eq!(
                left.span(right),
                SourceInfo {
                    start: 0,
                    length: 12
                }
            )
        }
    }
}

mod token {
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
        value: String,
    }

    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Default)]
    pub struct IntLiteral {
        pub value: u128,
    }
}

mod lexer {
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
                '&' => self.adv_next(TokKind::And),
                '@' => self.adv_next(TokKind::At),
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
}

mod ast {
    use crate::source_info::SourceInfo;
    use crate::token::IntLiteral;

    #[derive(Clone, Debug, PartialEq, Eq)]
    pub struct Expr {
        pub kind: ExprKind,
        pub source_info: SourceInfo,
    }

    #[derive(Clone, Debug, PartialEq, Eq)]
    pub enum ExprKind {
        IntLiteral(IntLiteral),
        UnaryOp(Box<UnaryOp>),
        BinaryOp(Box<BinaryOp>),
    }

    #[derive(Clone, Debug, PartialEq, Eq)]
    pub struct UnaryOp {
        pub operator: UnOpKind,
        pub expr: Expr,
    }

    #[derive(Copy, Clone, Debug, PartialEq, Eq)]
    pub enum UnOpKind {
        Ref,
        Deref,
    }

    #[derive(Clone, Debug, PartialEq, Eq)]
    pub struct BinaryOp {
        pub operator: BinOpKind,
        pub left: Expr,
        pub right: Expr,
    }

    #[derive(Copy, Clone, Debug, PartialEq, Eq)]
    pub enum BinOpKind {
        Add,
        Mult,
    }
}

mod parser {
    use crate::ast::*;
    use crate::source_info::SourceInfo;
    use crate::token::{Tok, TokKind};

    #[derive(Clone, Debug, PartialEq, Eq)]
    pub struct Parser {
        tokens: Vec<Tok>,
        index: usize,
    }

    #[derive(Clone, Debug, PartialEq, Eq)]
    pub struct ParseError {
        pub kind: ParseErrKind,
        pub source_info: SourceInfo,
    }

    impl ParseError {
        pub fn expected_token(tok: TokKind, source_info: SourceInfo) -> Self {
            Self {
                kind: ParseErrKind::ExpectedToken(tok),
                source_info,
            }
        }
        pub fn expected(name: &str, source_info: SourceInfo) -> Self {
            Self {
                kind: ParseErrKind::Expected(String::from(name)),
                source_info,
            }
        }
    }

    #[derive(Clone, Debug, PartialEq, Eq)]
    pub enum ParseErrKind {
        Expected(String),
        ExpectedToken(TokKind),
    }

    type Parse<T> = Result<T, ParseError>;
    type ParseOpt<T> = Result<Option<T>, ParseError>;

    impl Parser {
        pub fn parse_expr(tokens: Vec<Tok>) -> Parse<Expr> {
            let mut parser = Self::new(tokens);
            let expr = parser.expect_p("expr", Self::expr);
            parser.expect_token(TokKind::EndOfInput)?;
            expr
        }

        fn new(tokens: Vec<Tok>) -> Self {
            Self { tokens, index: 0 }
        }
        fn peek(&self) -> TokKind {
            self.tokens
                .get(self.index)
                .map(|t| t.kind.clone())
                .expect("token")
        }

        fn peek_source(&self) -> SourceInfo {
            self.tokens
                .get(self.index)
                .map(|t| t.source_info)
                .expect("token")
        }

        fn advance(&mut self) {
            self.index += 1
        }

        fn expect_p<T>(&mut self, name: &str, get_value: fn(&mut Self) -> ParseOpt<T>) -> Parse<T> {
            let val = get_value(self);
            match val {
                Ok(Some(value)) => Ok(value),
                Ok(None) => Err(ParseError::expected(name, self.peek_source())),
                Err(e) => Err(e),
            }
        }

        fn expect_token(&mut self, tok: TokKind) -> Parse<()> {
            if self.peek() == tok {
                self.advance();
                Ok(())
            } else {
                Err(ParseError::expected_token(tok, self.peek_source()))
            }
        }

        fn left_op_expr(
            &mut self,
            parse_operand: fn(&mut Self) -> ParseOpt<Expr>,
            parse_operator: fn(&mut Self) -> ParseOpt<BinOpKind>,
        ) -> ParseOpt<Expr> {
            if let Some(mut left) = parse_operand(self)? {
                loop {
                    if let Some(operator) = parse_operator(self)? {
                        let right = self.expect_p("expr", parse_operand)?;
                        let source_info = left.source_info.span(right.source_info);
                        left = Expr {
                            kind: ExprKind::BinaryOp(Box::new(BinaryOp {
                                operator,
                                left,
                                right,
                            })),
                            source_info,
                        };
                    } else {
                        return Ok(Some(left));
                    }
                }
            }
            Ok(None)
        }

        fn expr(&mut self) -> ParseOpt<Expr> {
            self.add_expr()
        }

        fn add_expr(&mut self) -> ParseOpt<Expr> {
            self.left_op_expr(Self::mult_expr, |p| match p.peek() {
                TokKind::Plus => {
                    p.advance();
                    Ok(Some(BinOpKind::Add))
                }
                _ => Ok(None),
            })
        }

        fn mult_expr(&mut self) -> ParseOpt<Expr> {
            self.left_op_expr(Self::un_op_expr, |p| match p.peek() {
                TokKind::Star => {
                    p.advance();
                    Ok(Some(BinOpKind::Mult))
                }
                _ => Ok(None),
            })
        }

        fn un_op_expr(&mut self) -> ParseOpt<Expr> {
            let op_source = self.peek_source();
            let operator = match self.peek() {
                TokKind::And => {
                    self.advance();
                    UnOpKind::Ref
                }
                TokKind::At => {
                    self.advance();
                    UnOpKind::Deref
                }
                _ => return self.base_expr(),
            };
            let expr = self.expect_p("expr", Self::un_op_expr)?;
            let source_info = op_source.span(expr.source_info);
            Ok(Some(Expr {
                kind: ExprKind::UnaryOp(Box::new(UnaryOp { operator, expr })),
                source_info,
            }))
        }

        fn base_expr(&mut self) -> ParseOpt<Expr> {
            match self.peek() {
                TokKind::ParLeft => {
                    self.advance();
                    let expr = self.expect_p("expr", Self::expr)?;
                    self.expect_token(TokKind::ParRight)?;
                    Ok(Some(expr))
                }
                TokKind::IntLiteral(payload) => {
                    let source_info = self.peek_source();
                    self.advance();
                    Ok(Some(Expr {
                        kind: ExprKind::IntLiteral(payload),
                        source_info,
                    }))
                }
                _ => Ok(None),
            }
        }
    }

    #[cfg(test)]
    mod test {
        use super::*;
        use crate::lexer::Lexer;
        use crate::token::IntLiteral;

        fn assert_expr_eq(str: &str, expected: Expr) {
            let result = Parser::parse_expr(Lexer::lex(str)).expect("expr");
            assert_eq!(result, expected);
        }

        fn assert_err(str: &str, expected: ParseError) {
            let result = Parser::parse_expr(Lexer::lex(str)).expect_err("parse error");
            assert_eq!(result, expected);
        }

        #[test]
        fn parse_op_expr() {
            assert_expr_eq(
                "  1 + 2 ",
                Expr {
                    kind: ExprKind::BinaryOp(Box::new(BinaryOp {
                        operator: BinOpKind::Add,
                        left: Expr {
                            kind: ExprKind::IntLiteral(IntLiteral { value: 1 }),
                            source_info: SourceInfo {
                                start: 2,
                                length: 1,
                            },
                        },
                        right: Expr {
                            kind: ExprKind::IntLiteral(IntLiteral { value: 2 }),
                            source_info: SourceInfo {
                                start: 6,
                                length: 1,
                            },
                        },
                    })),
                    source_info: SourceInfo {
                        start: 2,
                        length: 5,
                    },
                },
            )
        }

        #[test]
        fn missing_end_paren() {
            assert_err(
                "(123",
                ParseError::expected_token(
                    TokKind::ParRight,
                    SourceInfo {
                        start: 4,
                        length: 0,
                    },
                ),
            )
        }
    }
}

mod runtime {
    use crate::ast::{BinOpKind, Expr, ExprKind, UnOpKind};

    // FIXME
    type Word = u128;

    #[derive(Clone, Debug, PartialEq, Eq)]
    struct RuntimeError {
        kind: RunErrKind,
    }

    #[derive(Clone, Debug, PartialEq, Eq)]
    enum RunErrKind {}

    #[derive(Clone, Debug, PartialEq, Eq)]
    pub struct Interpreter {
        sp: usize,
        memory: Vec<Word>,
    }

    impl Interpreter {
        pub fn new() -> Self {
            let size = 128;
            Self {
                sp: size - 1,
                memory: vec![0; size],
            }
        }
        pub fn poke(&mut self, index: usize, value: Word) {
            self.memory[index] = value;
        }
        pub fn peek(&self, index: usize) -> Word {
            self.memory[index]
        }
        pub fn push_stk(&mut self, value: Word) {
            self.memory[self.sp] = value;
            self.sp -= 1;
        }
        pub fn top_stk(&mut self) -> Word {
            self.memory[self.sp + 1]
        }
        pub fn pop_stk(&mut self) -> Word {
            self.sp += 1;
            self.memory[self.sp]
        }
        pub fn eval_expr(&mut self, expr: Expr) {
            match expr.kind {
                ExprKind::IntLiteral(payload) => {
                    self.push_stk(payload.value);
                }
                ExprKind::BinaryOp(payload) => {
                    self.eval_expr(payload.left);
                    self.eval_expr(payload.right);
                    let right = self.pop_stk();
                    let left = self.pop_stk();
                    let val = match payload.operator {
                        BinOpKind::Add => left + right,
                        BinOpKind::Mult => left * right,
                    };
                    self.push_stk(val);
                }
                ExprKind::UnaryOp(payload) => match payload.operator {
                    UnOpKind::Ref => unimplemented!(),
                    UnOpKind::Deref => {
                        self.eval_expr(payload.expr);
                        let val = self.pop_stk();
                        self.push_stk(self.peek(val as usize));
                    }
                },
            };
        }
    }

    #[cfg(test)]
    mod test {
        use super::*;
        #[test]
        fn push_pop() {
            let mut interpreter = Interpreter::new();
            interpreter.push_stk(123);
            assert_eq!(interpreter.pop_stk(), 123);
        }
    }
}

use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::runtime::Interpreter;

fn main() {
    println!("Hello, world!");
    Interpreter::new().eval_expr(Parser::parse_expr(Lexer::lex("123")).unwrap());
}

#[cfg(test)]
mod test {
    use super::*;

    fn assert_expr_eq(str: &str, expected: u128) {
        let mut interpreter = Interpreter::new();
        interpreter.eval_expr(Parser::parse_expr(Lexer::lex(str)).expect("expr"));

        assert_eq!(interpreter.pop_stk(), expected);
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

    #[test]
    fn add_mult() {
        assert_expr_eq("2 * 4 + 5", 13);
    }

    #[test]
    fn add_mult_paren() {
        assert_expr_eq("2 * (4 + 5)", 18);
    }

    #[test]
    fn add_mult_pemdas() {
        assert_expr_eq("5 + 4 * 2", 13);
    }
}
