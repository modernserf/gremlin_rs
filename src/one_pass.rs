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
    Plus,
    Minus,
    Star,
    ParLeft,
    ParRight,
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
                '+' => self.operator(Token::Plus),
                '-' => self.operator(Token::Minus),
                '*' => self.operator(Token::Star),
                '(' => self.operator(Token::ParLeft),
                ')' => self.operator(Token::ParRight),
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
    fn check(&self, other: Ty) -> Compile<Ty> {
        if self == &other {
            Ok(other)
        } else {
            Err(CompileError::ExpectedType(self.clone(), other))
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
    fn to_dest(&self) -> Dest {
        Dest::FrameOffset(self.frame_offset)
    }
    fn to_src(&self) -> Src {
        Src::FrameOffset(self.frame_offset)
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

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Dest {
    Stack,
    FrameOffset(Word),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Src {
    Immediate(Word),
    FrameOffset(Word),
    PopStack,
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
    fn write(&mut self, op: IROp, dest: Dest, src: Src, dest_ty: Ty) -> ScopeRecord {
        let ir_src = match src {
            Src::Immediate(value) => IRSrc::Immediate(value),
            Src::FrameOffset(offset) => {
                let stack_offset = self.current_frame_size - offset;
                IRSrc::StackOffset(stack_offset)
            }
            Src::PopStack => {
                self.current_frame_size -= 1;
                IRSrc::PopStack
            }
        };
        let (ir_dest, result) = match dest {
            Dest::Stack => {
                self.current_frame_size += 1;
                (
                    IRDest::PushStack,
                    ScopeRecord::new(self.current_frame_size, dest_ty),
                )
            }
            Dest::FrameOffset(offset) => {
                let stack_offset = self.current_frame_size - offset;
                (
                    IRDest::StackOffset(stack_offset),
                    ScopeRecord::new(offset, dest_ty),
                )
            }
        };
        self.output.push(op(ir_dest, ir_src));
        result
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
            let rec = expr(state, ExprContext::let_binding(bind_ty))?
                .ok_or(CompileError::Expected("expr"))?;
            state.scope.assign(binding, rec);
            Ok(Some(()))
        }
        Token::Identifier(ident) => match lvalue(state)? {
            Some(rec) => {
                if token(state, Token::ColonEq)?.is_some() {
                    expr(state, ExprContext::assign(rec))?.ok_or(CompileError::Expected("expr"))?;
                    Ok(Some(()))
                } else {
                    let left = identifier(state, ExprContext::stack(), &ident)?;
                    op_expr(state, left)?;
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

fn lvalue(state: &mut State) -> CompileOpt<ScopeRecord> {
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
            Some(ty) => ty.check(other),
            None => Ok(other),
        }
    }
}

fn expr(state: &mut State, ctx: ExprContext) -> CompileOpt<ScopeRecord> {
    let left: ScopeRecord = match base_expr(state, ctx)? {
        Some(ty) => ty,
        None => return Ok(None),
    };
    op_expr(state, left)
}

struct OpParser {
    op_stack: Vec<Op>,
    operands: Vec<ScopeRecord>,
}

// a * b + c
// a: [a] []
// *: [a] [*]
// b: [a, b] [*]
// +: [a * b] [+]
// c: [a * b, c] [+]
// .. [(a * b) + c] []

// a + b * c
// a: [a] []
// +: [a] [+]
// b: [a, b] [+]
// *: [a, b] [+, *]
// c: [a, b, c] [+, *]
// .. [a, b * c] [+]
// .. [a + (b * c)] []

impl OpParser {
    fn new(left: ScopeRecord) -> Self {
        Self {
            op_stack: Vec::new(),
            operands: vec![left],
        }
    }
    fn apply(&mut self, state: &mut State, op: Op) -> Compile<()> {
        let right = self.operands.pop().expect("rhs");
        let left = self.operands.pop().expect("lhs");
        let dest = left.to_dest();
        let dest_ty = op.check_ty(left.ty, right.ty)?;
        let res = state.write(op.ir(), dest, Src::PopStack, dest_ty);
        self.operands.push(res);
        Ok(())
    }
    fn unwind(&mut self, state: &mut State) -> Compile<ScopeRecord> {
        while let Some(op) = self.op_stack.pop() {
            self.apply(state, op)?;
        }
        Ok(self.operands.pop().expect("op result"))
    }
    fn next(&mut self, state: &mut State, op: Op) -> Compile<()> {
        match self.op_stack.pop() {
            Some(last_op) => {
                if last_op.precedence() > op.precedence() {
                    self.op_stack.push(last_op);
                    self.op_stack.push(op);
                } else {
                    self.apply(state, last_op)?;
                    self.op_stack.push(op);
                }
            }
            None => {
                self.op_stack.push(op);
            }
        };
        Ok(())
    }
    fn push_rhs(&mut self, rhs: ScopeRecord) {
        self.operands.push(rhs);
    }
}

fn op_expr(state: &mut State, left: ScopeRecord) -> CompileOpt<ScopeRecord> {
    let mut op_parser = OpParser::new(left);

    loop {
        match operator(state)? {
            Some(op) => {
                op_parser.next(state, op)?;
                let res = base_expr(state, ExprContext::stack())?
                    .ok_or(CompileError::Expected("expr"))?;
                op_parser.push_rhs(res);
            }
            None => return op_parser.unwind(state).map(Some),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Op {
    Add,
    Sub,
    Mul,
}

impl Op {
    fn precedence(&self) -> usize {
        match self {
            Op::Add => 1,
            Op::Sub => 1,
            Op::Mul => 0,
        }
    }
    fn check_ty(&self, left: Ty, right: Ty) -> Compile<Ty> {
        match self {
            Op::Add => self.arithmetic(left, right),
            Op::Sub => self.arithmetic(left, right),
            Op::Mul => self.arithmetic(left, right),
        }
    }
    fn arithmetic(&self, left: Ty, right: Ty) -> Compile<Ty> {
        Ty::int().check(left)?;
        Ty::int().check(right)?;
        Ok(Ty::int())
    }

    fn ir(&self) -> IROp {
        match self {
            Op::Add => IR::add,
            Op::Sub => IR::sub,
            Op::Mul => IR::mult,
        }
    }
}

fn operator(state: &mut State) -> CompileOpt<Op> {
    let op = match state.lexer.peek()? {
        Token::Plus => Op::Add,
        Token::Minus => Op::Sub,
        Token::Star => Op::Mul,
        _ => return Ok(None),
    };
    state.lexer.advance();
    Ok(Some(op))
}

fn base_expr(state: &mut State, ctx: ExprContext) -> CompileOpt<ScopeRecord> {
    match state.lexer.peek()? {
        Token::ParLeft => {
            state.lexer.advance();
            let res = expr(state, ctx)?;
            expect_token(state, Token::ParRight)?;
            return Ok(res);
        }
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

fn number(state: &mut State, ctx: ExprContext, value: Word) -> Compile<ScopeRecord> {
    let ty = ctx.check_ty(Ty::int())?;
    Ok(state.write(IR::mov, ctx.dest, Src::Immediate(value), ty))
}

fn boolean(state: &mut State, ctx: ExprContext, value: bool) -> Compile<ScopeRecord> {
    let ty = ctx.check_ty(Ty::bool())?;
    let src = Src::Immediate(if value { 1 } else { 0 });
    Ok(state.write(IR::mov, ctx.dest, src, ty))
}

fn identifier(state: &mut State, ctx: ExprContext, name: &str) -> Compile<ScopeRecord> {
    let record = state.scope.get(name)?;
    let src = record.to_src();
    let ty = ctx.check_ty(record.ty)?;
    Ok(state.write(IR::mov, ctx.dest, src, ty))
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

    #[test]
    fn addition() {
        expect_ir(
            "
            let x := 1;
            let y := 2;
            x + 3 + y 
        ",
            vec![
                IR::mov(IRDest::PushStack, IRSrc::Immediate(1)), //     [x: 1]
                IR::mov(IRDest::PushStack, IRSrc::Immediate(2)), //     [y: 2, x: 1]
                IR::mov(IRDest::PushStack, IRSrc::StackOffset(1)), //   [1, y: 2, x: 1]
                IR::mov(IRDest::PushStack, IRSrc::Immediate(3)), //     [3, 1, y: 2, x: 1]
                IR::add(IRDest::StackOffset(0), IRSrc::PopStack), //    [4, y: 2, x: 1]
                IR::mov(IRDest::PushStack, IRSrc::StackOffset(1)), //   [2, 4, y: 2, x: 1]
                IR::add(IRDest::StackOffset(0), IRSrc::PopStack), //    [6, y: 2, x: 1]
            ],
        )
    }

    #[test]
    fn parens() {
        expect_ir(
            "3 * (4 + 5)",
            vec![
                IR::mov(IRDest::PushStack, IRSrc::Immediate(3)),
                IR::mov(IRDest::PushStack, IRSrc::Immediate(4)),
                IR::mov(IRDest::PushStack, IRSrc::Immediate(5)),
                IR::add(IRDest::StackOffset(0), IRSrc::PopStack),
                IR::mult(IRDest::StackOffset(0), IRSrc::PopStack),
            ],
        )
    }

    #[test]
    fn precedence() {
        expect_ir(
            "4 + 5 * 3",
            vec![
                IR::mov(IRDest::PushStack, IRSrc::Immediate(4)),
                IR::mov(IRDest::PushStack, IRSrc::Immediate(5)),
                IR::mov(IRDest::PushStack, IRSrc::Immediate(3)),
                IR::mult(IRDest::StackOffset(0), IRSrc::PopStack),
                IR::add(IRDest::StackOffset(0), IRSrc::PopStack),
            ],
        )
    }
}
