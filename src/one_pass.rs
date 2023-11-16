use std::collections::HashMap;

use crate::ir::{IRDest, IRSrc, Word, IR};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CompileError {
    UnexpectedChar,
    ExpectedToken(Token),
    UnknownIdentifier(String),
    ExpectedType(Ty, Ty),
    UnknownTypeIdentifier(String),
    Expected(&'static str),
}

type Compile<T> = Result<T, CompileError>;
type CompileOpt<T> = Result<Option<T>, CompileError>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    Integer(Word),
    Identifier(String),
    EndOfInput,
    True,
    False,
    Let,
    Colon,
    Semicolon,
    ColonEq,
}

mod lexer {
    use super::*;
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
                self.current_token = self.token();
            }
            self.current_token.clone()
        }
        pub fn advance(&mut self) {
            self.current_token = self.token()
        }
        fn token(&mut self) -> Compile<Token> {
            match self.peek_char() {
                ' ' | '\n' => self.whitespace(),
                '#' => self.comment(),
                '\0' => Ok(Token::EndOfInput),
                '0'..='9' => self.number(),
                'l' => self.keyword("let", Token::Let),
                't' => self.keyword("true", Token::True),
                'f' => self.keyword("false", Token::False),
                ';' => self.operator(Token::Semicolon),
                ':' => {
                    self.adv_char();
                    match self.peek_char() {
                        '=' => self.operator(Token::ColonEq),
                        _ => Ok(Token::Colon),
                    }
                }
                'a'..='z' => self.identifier(""),
                _ => Err(CompileError::UnexpectedChar),
            }
        }
        fn whitespace(&mut self) -> Compile<Token> {
            loop {
                if self.peek_char().is_whitespace() {
                    self.adv_char();
                } else {
                    return self.token();
                }
            }
        }
        fn comment(&mut self) -> Compile<Token> {
            loop {
                let next = self.peek_char();
                if next == '\n' || next == '\0' {
                    return self.token();
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
            for (i, ch) in keyword.chars().enumerate() {
                if self.peek_char() != ch {
                    return self.identifier(&keyword[0..i]);
                }
                self.adv_char()
            }

            Ok(token)
        }
        fn identifier(&mut self, prefix: &str) -> Compile<Token> {
            let mut out = String::from(prefix);
            loop {
                let ch = self.peek_char();
                if ch.is_alphanumeric() || ch == '_' {
                    self.adv_char();
                    out.push(ch);
                } else {
                    return Ok(Token::Identifier(out));
                }
            }
        }
        fn operator(&mut self, token: Token) -> Compile<Token> {
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
}
use lexer::Lexer;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ty {
    kind: TyKind,
}

impl Ty {
    fn int() -> Self {
        Ty { kind: TyKind::Int }
    }
    fn bool() -> Self {
        Ty { kind: TyKind::Bool }
    }
    fn check(&self, other: &Ty) -> Compile<()> {
        if self == other {
            Ok(())
        } else {
            Err(CompileError::ExpectedType(self.clone(), other.clone()))
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum TyKind {
    Int,
    Bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct ScopeRecord {
    frame_offset: Word,
    ty: Ty,
}
impl ScopeRecord {
    fn new(frame_offset: Word, ty: Ty) -> Self {
        Self { frame_offset, ty }
    }
}

struct Scope {
    data: HashMap<String, ScopeRecord>,
}

impl Scope {
    fn new() -> Self {
        Self {
            data: HashMap::new(),
        }
    }
    fn get(&self, key: &str) -> Compile<ScopeRecord> {
        self.data
            .get(key)
            .map(|t| t.clone())
            .ok_or_else(|| CompileError::UnknownIdentifier(key.to_string()))
    }
    fn assign(&mut self, key: String, record: ScopeRecord) {
        self.data.insert(key, record);
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Dest {
    Stack,
    FrameOffset(Word),
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Src {
    Immediate(Word),
    FrameOffset(Word),
}

type IROp = fn(IRDest, IRSrc) -> IR;

struct State {
    lexer: Lexer,
    output: Vec<IR>,
    scope: Scope,
    current_frame_size: Word,
}

impl State {
    fn new(str: &str) -> Self {
        Self {
            lexer: Lexer::new(str),
            output: Vec::new(),
            scope: Scope::new(),
            current_frame_size: 0,
        }
    }
    fn write(&mut self, op: IROp, dest: Dest, src: Src) {
        let ir_src = match src {
            Src::Immediate(value) => IRSrc::Immediate(value),
            Src::FrameOffset(offset) => {
                let stack_offset = self.current_frame_size - offset;
                IRSrc::StackOffset(stack_offset)
            }
        };
        let ir_dest = match dest {
            Dest::Stack => {
                self.current_frame_size += 1;
                IRDest::PushStack
            }
            Dest::FrameOffset(offset) => {
                let stack_offset = self.current_frame_size - offset;
                IRDest::StackOffset(stack_offset)
            }
        };
        self.output.push(op(ir_dest, ir_src));
    }
}

#[allow(dead_code)]
fn program(input: &str) -> Compile<Vec<IR>> {
    let mut state = State::new(input);
    alternating(&mut state, stmt, |s| token(s, Token::Semicolon))?;
    token(&mut state, Token::EndOfInput)?.ok_or(CompileError::ExpectedToken(Token::EndOfInput))?;
    Ok(state.output)
}

fn token(state: &mut State, token: Token) -> CompileOpt<()> {
    if state.lexer.peek()? == token {
        state.lexer.advance();
        return Ok(Some(()));
    }
    return Ok(None);
}

fn expect_token(state: &mut State, tok: Token) -> Compile<()> {
    token(state, tok.clone())?.ok_or(CompileError::ExpectedToken(tok))
}

fn binding(state: &mut State) -> CompileOpt<String> {
    match state.lexer.peek()? {
        Token::Identifier(str) => {
            state.lexer.advance();
            Ok(Some(str))
        }
        _ => Ok(None),
    }
}

type Syntax<T> = fn(state: &mut State) -> CompileOpt<T>;
fn alternating<T, U>(state: &mut State, item: Syntax<T>, separator: Syntax<U>) -> Compile<()> {
    loop {
        if item(state)?.is_none() {
            return Ok(());
        }
        if separator(state)?.is_none() {
            return Ok(());
        }
    }
}

fn stmt(state: &mut State) -> CompileOpt<()> {
    match state.lexer.peek()? {
        Token::Let => {
            state.lexer.advance();
            let binding = binding(state)?.ok_or(CompileError::Expected("binding"))?;
            let mut bind_ty = None;

            if token(state, Token::Colon)?.is_some() {
                let ty = type_expr(state)?.ok_or(CompileError::Expected("type expr"))?;
                bind_ty = Some(ty)
            }
            expect_token(state, Token::ColonEq)?;
            let expr_ty = expr(state, ExprContext::let_binding(bind_ty))?
                .ok_or(CompileError::Expected("expr"))?;
            state
                .scope
                .assign(binding, ScopeRecord::new(state.current_frame_size, expr_ty));
            Ok(Some(()))
        }
        Token::Identifier(ident) => match lvalue(state, ExprContext::stack())? {
            Some(rec) => {
                if token(state, Token::ColonEq)?.is_some() {
                    expr(state, ExprContext::assign(rec))?.ok_or(CompileError::Expected("expr"))?;
                    Ok(Some(()))
                } else {
                    identifier(state, ExprContext::stack(), &ident)?;
                    Ok(Some(()))
                }
            }
            None => unreachable!(),
        },
        _ => {
            expr(state, ExprContext::stack())?;
            Ok(Some(()))
        }
    }
}

fn type_expr(state: &mut State) -> CompileOpt<Ty> {
    match state.lexer.peek()? {
        Token::Identifier(ident) => match ident.as_str() {
            "int" => {
                state.lexer.advance();
                Ok(Some(Ty::int()))
            }
            "bool" => {
                state.lexer.advance();
                Ok(Some(Ty::bool()))
            }
            _ => Err(CompileError::UnknownTypeIdentifier(ident)),
        },
        _ => Ok(None),
    }
}

// #[derive(Clone, Debug, PartialEq, Eq)]
// enum LValue {
//     LValue(ScopeRecord),
//     Expr(Ty),
// }

fn lvalue(state: &mut State, ctx: ExprContext) -> Compile<Option<ScopeRecord>> {
    match state.lexer.peek()? {
        Token::Identifier(ident) => {
            state.lexer.advance();
            let record = state.scope.get(&ident)?;
            Ok(Some(record))
        }
        _ => Ok(None),
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct ExprContext {
    ty: Option<Ty>,
    dest: Dest,
}

impl ExprContext {
    fn stack() -> Self {
        Self {
            ty: None,
            dest: Dest::Stack,
        }
    }
    fn let_binding(ty: Option<Ty>) -> Self {
        Self {
            ty,
            dest: Dest::Stack,
        }
    }
    fn assign(rec: ScopeRecord) -> Self {
        Self {
            ty: Some(rec.ty),
            dest: Dest::FrameOffset(rec.frame_offset),
        }
    }
    fn check_ty(&self, other: Ty) -> Compile<Ty> {
        match &self.ty {
            Some(ty) => {
                ty.check(&other)?;
                Ok(other)
            }
            None => Ok(other),
        }
    }
}

fn expr(state: &mut State, ctx: ExprContext) -> CompileOpt<Ty> {
    match state.lexer.peek()? {
        Token::Integer(int) => {
            state.lexer.advance();
            number(state, ctx, int)
        }
        Token::True => {
            state.lexer.advance();
            boolean(state, ctx, true)
        }
        Token::False => {
            state.lexer.advance();
            boolean(state, ctx, false)
        }
        Token::Identifier(name) => {
            state.lexer.advance();
            identifier(state, ctx, &name)
        }
        _ => return Ok(None),
    }
    .map(Some)
}

fn number(state: &mut State, ctx: ExprContext, value: Word) -> Compile<Ty> {
    let ty = ctx.check_ty(Ty::int())?;
    state.write(IR::mov, ctx.dest, Src::Immediate(value));
    Ok(ty)
}

fn boolean(state: &mut State, ctx: ExprContext, value: bool) -> Compile<Ty> {
    let ty = ctx.check_ty(Ty::bool())?;
    let src = Src::Immediate(if value { 1 } else { 0 });
    state.write(IR::mov, ctx.dest, src);
    Ok(ty)
}

fn identifier(state: &mut State, ctx: ExprContext, name: &str) -> Compile<Ty> {
    let record = state.scope.get(name)?;
    let ty = ctx.check_ty(record.ty)?;
    state.write(IR::mov, ctx.dest, Src::FrameOffset(record.frame_offset));
    Ok(ty)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ir::IR;
    fn expect_ir(code: &str, ir: Vec<IR>) {
        assert_eq!(program(code), Ok(ir))
    }
    fn expect_err(code: &str, err: CompileError) {
        assert_eq!(program(code), Err(err))
    }

    #[test]
    fn empty_program() {
        expect_ir("", vec![]);
    }

    #[test]
    fn integers() {
        expect_ir(
            "123",
            vec![IR::mov(IRDest::PushStack, IRSrc::Immediate(123))],
        )
    }

    #[test]
    fn whitespace() {
        expect_ir(
            "  
            123
            
            ",
            vec![IR::mov(IRDest::PushStack, IRSrc::Immediate(123))],
        )
    }

    #[test]
    fn comments() {
        expect_ir(
            "123 # This is a comment",
            vec![IR::mov(IRDest::PushStack, IRSrc::Immediate(123))],
        )
    }

    #[test]
    fn unexpected_char() {
        expect_err(" £ ", CompileError::UnexpectedChar)
    }

    #[test]
    fn bools() {
        expect_ir(
            "true",
            vec![IR::mov(IRDest::PushStack, IRSrc::Immediate(1))],
        );
        expect_ir(
            "false",
            vec![IR::mov(IRDest::PushStack, IRSrc::Immediate(0))],
        );
    }

    #[test]
    fn sequences() {
        expect_ir(
            "
            123;
            true
        ",
            vec![
                IR::mov(IRDest::PushStack, IRSrc::Immediate(123)),
                IR::mov(IRDest::PushStack, IRSrc::Immediate(1)),
            ],
        )
    }

    #[test]
    fn let_stmts() {
        expect_ir(
            "
                let x := 1;
                let y := 2;
                x
            ",
            vec![
                IR::mov(IRDest::PushStack, IRSrc::Immediate(1)),
                IR::mov(IRDest::PushStack, IRSrc::Immediate(2)),
                IR::mov(IRDest::PushStack, IRSrc::StackOffset(1)),
            ],
        )
    }

    #[test]
    fn type_exprs() {
        expect_ir(
            "
                let x : int := 1;
            ",
            vec![IR::mov(IRDest::PushStack, IRSrc::Immediate(1))],
        );
        expect_err(
            "
            let x : bool := 1;
        ",
            CompileError::ExpectedType(Ty::bool(), Ty::int()),
        )
    }

    #[test]
    fn assignment() {
        expect_ir(
            "
            let x := 1;
            let y := 3;
            x := y;
            x
        ",
            vec![
                IR::mov(IRDest::PushStack, IRSrc::Immediate(1)),
                IR::mov(IRDest::PushStack, IRSrc::Immediate(3)),
                IR::mov(IRDest::StackOffset(1), IRSrc::StackOffset(0)),
                IR::mov(IRDest::PushStack, IRSrc::StackOffset(1)),
            ],
        )
    }
}
