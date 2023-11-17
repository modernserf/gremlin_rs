use std::collections::HashMap;

// big list of optimizations I'm not doing yet
// - constant folding
// - registers
// - direct writes from expr to assign target
// TODO: fuse sequential [(push,x), (y,pop)] IR into [(y, x)]

pub type Word = i32;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum IRDest {
    PushStack,
    R0,
    SP,
    StackOffset(Word),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum IRSrc {
    Immediate(Word),
    StackOffset(Word),
    R0,
    R0Offset(Word),
    PopStack,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IR {
    Mov(IRDest, IRSrc),
    LoadAddress(IRDest, IRSrc),
    Add(IRDest, IRSrc),
    Sub(IRDest, IRSrc),
    Mult(IRDest, IRSrc),
    And(IRDest, IRSrc),
    Or(IRDest, IRSrc),
    Xor(IRDest, IRSrc),
    BitTest(IRDest, IRSrc),
}

pub struct Runtime {
    r0: Word,
    sp: Word,
    ip: usize,
    memory: Vec<Word>,
}

impl Runtime {
    pub fn eval(program: &[IR]) -> Word {
        let mut runtime = Self::new(128);
        runtime.run_program(program);
        runtime.memory[runtime.sp as usize]
    }
    fn new(memory_size: Word) -> Self {
        Self {
            r0: 0,
            sp: memory_size,
            ip: 0,
            memory: vec![0; memory_size as usize],
        }
    }
    fn run_program(&mut self, program: &[IR]) {
        while self.ip < program.len() {
            let instruction = &program[self.ip];
            self.ip += 1;
            self.run_instr(instruction);
        }
    }
    fn run_instr(&mut self, instr: &IR) {
        match &instr {
            IR::Mov(dest, src) => {
                let value = self.get_src(*src);
                let dest_ptr = self.get_dest(*dest);
                *dest_ptr = value;
            }
            IR::LoadAddress(dest, src) => {
                let effective_address = self.get_effective_address(*src);
                let dest_ptr = self.get_dest(*dest);
                *dest_ptr = effective_address;
            }
            IR::Add(dest, src) => {
                let value = self.get_src(*src);
                let dest_ptr = self.get_dest(*dest);
                *dest_ptr += value;
            }
            IR::Sub(dest, src) => {
                let value = self.get_src(*src);
                let dest_ptr = self.get_dest(*dest);
                *dest_ptr -= value;
            }
            IR::Mult(dest, src) => {
                let value = self.get_src(*src);
                let dest_ptr = self.get_dest(*dest);
                *dest_ptr *= value;
            }
            IR::Xor(dest, src) => {
                let value = self.get_src(*src);
                let dest_ptr = self.get_dest(*dest);
                *dest_ptr ^= value;
            }
            IR::And(dest, src) => {
                let value = self.get_src(*src);
                let dest_ptr = self.get_dest(*dest);
                *dest_ptr &= value;
            }
            IR::Or(dest, src) => {
                let value = self.get_src(*src);
                let dest_ptr = self.get_dest(*dest);
                *dest_ptr |= value;
            }
            // TODO: use a status register instead of r0
            IR::BitTest(dest, src) => {
                let bit = self.get_src(*src);
                let dest = *self.get_dest(*dest);
                self.r0 = if (dest & (1 << bit)) > 0 { 1 } else { 0 }
            }
        }
    }
    fn get_src(&mut self, src: IRSrc) -> Word {
        match src {
            IRSrc::Immediate(value) => value,
            IRSrc::R0 => self.r0,
            IRSrc::R0Offset(offset) => self.memory[(self.r0 + offset) as usize],
            IRSrc::StackOffset(offset) => self.memory[(self.sp + offset) as usize],
            IRSrc::PopStack => {
                let value = self.memory[self.sp as usize];
                self.sp += 1;
                value
            }
        }
    }
    fn get_effective_address(&self, src: IRSrc) -> Word {
        match src {
            IRSrc::StackOffset(offset) => self.sp + offset,
            _ => unimplemented!(),
        }
    }
    fn get_dest<'a>(&'a mut self, dest: IRDest) -> &'a mut Word {
        match dest {
            IRDest::R0 => &mut self.r0,
            IRDest::SP => &mut self.sp,
            IRDest::StackOffset(offset) => &mut self.memory[(self.sp + offset) as usize],
            IRDest::PushStack => {
                self.sp -= 1;
                &mut self.memory[self.sp as usize]
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CompileError {
    UnexpectedChar,
    ExpectedToken(Token),
    UnknownIdentifier(String),
    ExpectedType(Ty, Ty),
    UnknownTypeIdentifier(String),
    Expected(&'static str),
    InvalidDeref,
    InvalidCast,
    DuplicateField,
    MissingField,
}

type Compile<T> = Result<T, CompileError>;
type CompileOpt<T> = Result<Option<T>, CompileError>;

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
    Colon,
    Semicolon,
    Comma,
    Dot,
    ColonEq,
    Plus,
    Minus,
    Star,
    At,
    Ampersand,
    ParLeft,
    ParRight,
    CurlyLeft,
    CurlyRight,
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
                't' => {
                    self.adv_char();
                    match self.peek_char() {
                        'r' => self.keyword_idx("true", 1, Token::True),
                        _ => self.keyword_idx("type", 1, Token::Type),
                    }
                }
                'f' => self.keyword("false", Token::False),
                'a' => self.keyword("as", Token::As),
                's' => self.keyword("struct", Token::Struct),
                ';' => self.operator(Token::Semicolon),
                ',' => self.operator(Token::Comma),
                '.' => self.operator(Token::Dot),
                '+' => self.operator(Token::Plus),
                '-' => self.operator(Token::Minus),
                '*' => self.operator(Token::Star),
                '@' => self.operator(Token::At),
                '&' => self.operator(Token::Ampersand),
                '(' => self.operator(Token::ParLeft),
                ')' => self.operator(Token::ParRight),
                '{' => self.operator(Token::CurlyLeft),
                '}' => self.operator(Token::CurlyRight),
                ':' => {
                    self.adv_char();
                    match self.peek_char() {
                        '=' => self.operator(Token::ColonEq),
                        _ => Ok(Token::Colon),
                    }
                }
                'a'..='z' => self.identifier("").map(Token::Identifier),
                'A'..='Z' => self.identifier("").map(Token::TypeIdentifier),
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
            self.keyword_idx(keyword, 0, token)
        }
        fn keyword_idx(
            &mut self,
            keyword: &str,
            start_index: usize,
            token: Token,
        ) -> Compile<Token> {
            for (i, ch) in keyword.chars().skip(start_index).enumerate() {
                if self.peek_char() != ch {
                    return self
                        .identifier(&keyword[0..(i + start_index)])
                        .map(Token::Identifier);
                }
                self.adv_char()
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
    ref_level: usize,
}

impl Ty {
    fn int() -> Self {
        Self {
            kind: TyKind::Int,
            ref_level: 0,
        }
    }
    fn bool() -> Self {
        Self {
            kind: TyKind::Bool,
            ref_level: 0,
        }
    }
    fn struct_(data: TyStruct) -> Self {
        Self {
            kind: TyKind::Struct(Box::new(data)),
            ref_level: 0,
        }
    }
    fn add_ref(&self) -> Self {
        Self {
            kind: self.kind.clone(),
            ref_level: self.ref_level + 1,
        }
    }
    fn deref(&self) -> Compile<Self> {
        if self.ref_level == 0 {
            Err(CompileError::InvalidDeref)
        } else {
            Ok(Self {
                kind: self.kind.clone(),
                ref_level: self.ref_level - 1,
            })
        }
    }
    fn check(&self, other: &Self) -> Compile<()> {
        if self == other {
            Ok(())
        } else {
            Err(CompileError::ExpectedType(self.clone(), other.clone()))
        }
    }
    fn cast(&self, other: Self) -> Compile<Self> {
        if self.size() != other.size() {
            Err(CompileError::InvalidCast)
        } else {
            Ok(other)
        }
    }
    fn size(&self) -> Word {
        if self.ref_level > 0 {
            return 1;
        }
        match &self.kind {
            TyKind::Int => 1,
            TyKind::Bool => 1,
            TyKind::Struct(s) => s.size,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum TyKind {
    Int,
    Bool,
    Struct(Box<TyStruct>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct TyStruct {
    id: usize,
    size: Word,
    fields: HashMap<String, StructField>,
}

impl TyStruct {
    fn new(id: usize) -> Self {
        Self {
            id,
            size: 0,
            fields: HashMap::new(),
        }
    }
    fn insert(&mut self, k: String, ty: Ty) -> Compile<()> {
        if self.fields.contains_key(&k) {
            return Err(CompileError::DuplicateField);
        }
        let size = ty.size();
        self.fields.insert(
            k,
            StructField {
                ty,
                offset: self.size,
            },
        );
        self.size += size;
        return Ok(());
    }
    fn get(&self, k: &str) -> Compile<&StructField> {
        self.fields.get(k).ok_or(CompileError::MissingField)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct StructField {
    ty: Ty,
    offset: Word,
}

impl StructField {
    fn from_location(&self, loc: &MemLocation) -> MemLocation {
        match loc.kind {
            MemLocationKind::FrameOffset(parent_offset) => {
                MemLocation::local(parent_offset - self.offset, self.ty.clone())
            }
            _ => unimplemented!(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct MemLocation {
    kind: MemLocationKind,
    ty: Ty,
}
impl MemLocation {
    fn local(frame_offset: Word, ty: Ty) -> Self {
        Self {
            kind: MemLocationKind::FrameOffset(frame_offset),
            ty,
        }
    }
    fn r0(ty: Ty) -> Self {
        assert!(ty.size() == 1);
        Self {
            kind: MemLocationKind::R0,
            ty,
        }
    }
    fn to_dest(&self) -> Dest {
        match self.kind {
            MemLocationKind::FrameOffset(offset) => Dest::FrameOffset(offset),
            MemLocationKind::R0 => Dest::R0,
        }
    }
    fn to_src(&self) -> Src {
        match self.kind {
            MemLocationKind::FrameOffset(offset) => Src::FrameOffset(offset),
            MemLocationKind::R0 => unimplemented!(),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum MemLocationKind {
    FrameOffset(Word),
    R0,
}

struct Scope {
    data: HashMap<String, MemLocation>,
}

impl Scope {
    fn new() -> Self {
        Self {
            data: HashMap::new(),
        }
    }
    fn get(&self, key: &str) -> Compile<MemLocation> {
        self.data
            .get(key)
            .map(|t| t.clone())
            .ok_or_else(|| CompileError::UnknownIdentifier(key.to_string()))
    }
    fn assign(&mut self, key: String, record: MemLocation) {
        self.data.insert(key, record);
    }
}

struct TyScope {
    data: HashMap<String, Ty>,
}

impl TyScope {
    fn new() -> Self {
        Self {
            data: HashMap::from_iter(
                vec![("Int", Ty::int()), ("Bool", Ty::bool())]
                    .into_iter()
                    .map(|(k, v)| (k.to_string(), v)),
            ),
        }
    }
    fn get(&self, key: &str) -> Compile<Ty> {
        self.data
            .get(key)
            .map(|t| t.clone())
            .ok_or_else(|| CompileError::UnknownTypeIdentifier(key.to_string()))
    }
    fn assign(&mut self, key: String, record: Ty) {
        self.data.insert(key, record);
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Dest {
    Stack,
    FrameOffset(Word),
    R0,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Src {
    Immediate(Word),
    FrameOffset(Word),
    PopStack,
    R0Offset(Word),
}

type IROp = fn(IRDest, IRSrc) -> IR;

struct State {
    lexer: Lexer,
    output: Vec<IR>,
    scope: Scope,
    ty_scope: TyScope,
    current_frame_size: Word,
    next_type_id: usize,
}

impl State {
    fn new(str: &str) -> Self {
        Self {
            lexer: Lexer::new(str),
            output: Vec::new(),
            scope: Scope::new(),
            ty_scope: TyScope::new(),
            current_frame_size: 0,
            next_type_id: 1,
        }
    }
    fn new_type_id(&mut self) -> usize {
        let id = self.next_type_id;
        self.next_type_id += 1;
        id
    }
    fn write(&mut self, op: IROp, dest: Dest, src: Src, dest_ty: Ty) -> MemLocation {
        if dest_ty.size() == 1 {
            return self.write_1(op, dest, src, dest_ty, 0);
        }
        let res = match dest {
            Dest::Stack => {
                let loc = self.allocate(&dest_ty);
                return self.write(op, loc.to_dest(), src, dest_ty);
            }
            Dest::FrameOffset(offset) => MemLocation::local(offset, dest_ty),
            Dest::R0 => MemLocation::r0(dest_ty),
        };

        for i in 0..res.ty.size() {
            self.write_1(op, dest, src, res.ty.clone(), i);
        }

        res
    }
    fn write_1(&mut self, op: IROp, dest: Dest, src: Src, dest_ty: Ty, i: Word) -> MemLocation {
        let ir_src = match src {
            Src::Immediate(value) => IRSrc::Immediate(value),
            Src::FrameOffset(offset) => {
                let stack_offset = self.current_frame_size - offset + i;
                IRSrc::StackOffset(stack_offset)
            }
            Src::PopStack => {
                self.current_frame_size -= 1;
                IRSrc::PopStack
            }
            Src::R0Offset(offset) => IRSrc::R0Offset(offset),
        };
        let (ir_dest, result) = match dest {
            Dest::Stack => {
                self.current_frame_size += 1;
                (
                    IRDest::PushStack,
                    MemLocation::local(self.current_frame_size, dest_ty),
                )
            }
            Dest::FrameOffset(offset) => {
                let stack_offset = self.current_frame_size - offset + i;
                (
                    IRDest::StackOffset(stack_offset),
                    MemLocation::local(offset, dest_ty),
                )
            }
            Dest::R0 => (IRDest::R0, MemLocation::r0(dest_ty)),
        };
        self.output.push(op(ir_dest, ir_src));
        result
    }
    fn allocate(&mut self, ty: &Ty) -> MemLocation {
        self.current_frame_size += ty.size();
        self.output
            .push(IR::Sub(IRDest::SP, IRSrc::Immediate(ty.size())));
        MemLocation::local(self.current_frame_size, ty.clone())
    }
}

struct OpParser {
    op_stack: Vec<Op>,
    operands: Vec<MemLocation>,
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
    fn new(left: MemLocation) -> Self {
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
    fn unwind(&mut self, state: &mut State) -> Compile<MemLocation> {
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
    fn push_rhs(&mut self, rhs: MemLocation) {
        self.operands.push(rhs);
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
        Ty::int().check(&left)?;
        Ty::int().check(&right)?;
        Ok(Ty::int())
    }

    fn ir(&self) -> IROp {
        match self {
            Op::Add => IR::Add,
            Op::Sub => IR::Sub,
            Op::Mul => IR::Mult,
        }
    }
}

// ### Particles

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

fn ident_token(state: &mut State) -> CompileOpt<String> {
    match state.lexer.peek()? {
        Token::Identifier(s) => {
            state.lexer.advance();
            Ok(Some(s))
        }
        _ => Ok(None),
    }
}

// ### Exprs

fn struct_expr(state: &mut State, name: &str) -> Compile<MemLocation> {
    let ty = state.ty_scope.get(name)?;
    let fields = match &ty.kind {
        TyKind::Struct(fields) => fields,
        _ => return Err(CompileError::Expected("struct")),
    };
    let base_res: MemLocation = state.allocate(&ty);
    expect_token(state, Token::CurlyLeft)?;
    loop {
        let field_name = match ident_token(state)? {
            Some(s) => s,
            None => break,
        };
        expect_token(state, Token::Colon)?;
        let field = fields.get(&field_name)?;
        let field_loc = field.from_location(&base_res);
        let value = expr(state)?.ok_or(CompileError::Expected("expr"))?;
        field.ty.check(&value.ty)?;
        state.write(
            IR::Mov,
            field_loc.to_dest(),
            Src::PopStack,
            field.ty.clone(),
        );

        if token(state, Token::Comma)?.is_none() {
            break;
        }
    }
    expect_token(state, Token::CurlyRight)?;
    Ok(base_res)
}

fn base_expr(state: &mut State) -> CompileOpt<MemLocation> {
    match state.lexer.peek()? {
        Token::ParLeft => {
            state.lexer.advance();
            let res = expr(state)?;
            expect_token(state, Token::ParRight)?;
            return Ok(res);
        }
        Token::TypeIdentifier(name) => {
            state.lexer.advance();
            struct_expr(state, &name)
        }
        Token::Integer(int) => {
            state.lexer.advance();
            Ok(state.write(IR::Mov, Dest::Stack, Src::Immediate(int), Ty::int()))
        }
        Token::True => {
            state.lexer.advance();
            Ok(state.write(IR::Mov, Dest::Stack, Src::Immediate(1), Ty::bool()))
        }
        Token::False => {
            state.lexer.advance();
            Ok(state.write(IR::Mov, Dest::Stack, Src::Immediate(0), Ty::bool()))
        }
        Token::Identifier(name) => {
            state.lexer.advance();
            let record = state.scope.get(&name)?;
            let src = record.to_src();
            Ok(state.write(IR::Mov, Dest::Stack, src, record.ty))
        }
        _ => return Ok(None),
    }
    .map(Some)
}

fn postfix_expr(state: &mut State) -> CompileOpt<MemLocation> {
    let mut left = match base_expr(state)? {
        Some(e) => e,
        None => return Ok(None),
    };
    loop {
        if token(state, Token::Dot)?.is_none() {
            return Ok(Some(left));
        }
        let fields = match &left.ty.kind {
            TyKind::Struct(fields) => fields,
            _ => return Err(CompileError::Expected("struct")),
        };
        let field_name = ident_token(state)?.ok_or(CompileError::Expected("field"))?;
        let field = fields.get(&field_name)?;
        left = state.write(
            IR::Mov,
            Dest::Stack,
            field.from_location(&left).to_src(),
            field.ty.clone(),
        );
        dbg!(&left);
    }
}

fn unary_op_expr(state: &mut State) -> CompileOpt<MemLocation> {
    match state.lexer.peek()? {
        Token::Minus => {
            state.lexer.advance();
            let tgt = state.write(IR::Mov, Dest::Stack, Src::Immediate(0), Ty::int());
            unary_op_expr(state)?.ok_or(CompileError::Expected("expr"))?;
            let out = state.write(IR::Sub, tgt.to_dest(), Src::PopStack, tgt.ty);

            Ok(Some(out))
        }
        Token::Ampersand => {
            state.lexer.advance();
            let res = lvalue(state)?.ok_or(CompileError::Expected("lvalue"))?;
            let dest_ty = res.ty.add_ref();
            let out = state.write(IR::LoadAddress, Dest::Stack, res.to_src(), dest_ty);
            Ok(Some(out))
        }
        Token::At => {
            state.lexer.advance();
            let res = if let Some(res) = lvalue(state)? {
                state.write(IR::Mov, Dest::R0, res.to_src(), res.ty)
            } else {
                let res = unary_op_expr(state)?.ok_or(CompileError::Expected("expr"))?;
                state.write(IR::Mov, Dest::R0, Src::PopStack, res.ty)
            };
            let dest_ty = res.ty.deref()?;
            let out = state.write(IR::Mov, Dest::Stack, Src::R0Offset(0), dest_ty);
            Ok(Some(out))
        }
        _ => postfix_expr(state),
    }
}

fn op_expr(state: &mut State, left: MemLocation) -> CompileOpt<MemLocation> {
    let mut op_parser = OpParser::new(left);

    loop {
        match operator(state)? {
            Some(op) => {
                op_parser.next(state, op)?;
                let res = unary_op_expr(state)?.ok_or(CompileError::Expected("expr"))?;
                op_parser.push_rhs(res);
            }
            None => {
                let mut rec = match op_parser.unwind(state).map(Some)? {
                    Some(rec) => rec,
                    None => return Ok(None),
                };
                if token(state, Token::As)?.is_none() {
                    return Ok(Some(rec));
                }
                let ty = type_expr(state)?.ok_or(CompileError::Expected("type expr"))?;
                rec.ty = rec.ty.cast(ty)?;
                return Ok(Some(rec));
            }
        }
    }
}

fn expr(state: &mut State) -> CompileOpt<MemLocation> {
    let left: MemLocation = match unary_op_expr(state)? {
        Some(ty) => ty,
        None => return Ok(None),
    };
    op_expr(state, left)
}

// ### Bindings, TypeExprs, etc.

fn binding(state: &mut State) -> CompileOpt<String> {
    match state.lexer.peek()? {
        Token::Identifier(str) => {
            state.lexer.advance();
            Ok(Some(str))
        }
        _ => Ok(None),
    }
}

fn lvalue(state: &mut State) -> CompileOpt<MemLocation> {
    let mut left = match ident_token(state)? {
        Some(ident) => state.scope.get(&ident)?,
        None => return Ok(None),
    };

    loop {
        if token(state, Token::Dot)?.is_none() {
            return Ok(Some(left));
        }
        let fields = match &left.ty.kind {
            TyKind::Struct(fields) => fields,
            _ => return Err(CompileError::Expected("struct")),
        };
        let field_name = ident_token(state)?.ok_or(CompileError::Expected("field"))?;
        let field = fields.get(&field_name)?;
        left = field.from_location(&left);
    }
}

fn type_binding(state: &mut State) -> CompileOpt<String> {
    match state.lexer.peek()? {
        Token::TypeIdentifier(str) => {
            state.lexer.advance();
            Ok(Some(str))
        }
        _ => Ok(None),
    }
}

fn type_expr(state: &mut State) -> CompileOpt<Ty> {
    match state.lexer.peek()? {
        Token::TypeIdentifier(ident) => {
            state.lexer.advance();
            state.ty_scope.get(&ident).map(Some)
        }
        Token::Struct => {
            state.lexer.advance();
            struct_ty(state).map(Some)
        }
        _ => Ok(None),
    }
}

fn struct_ty(state: &mut State) -> Compile<Ty> {
    let mut fields = TyStruct::new(state.new_type_id());

    expect_token(state, Token::CurlyLeft)?;
    loop {
        let key = match ident_token(state)? {
            Some(s) => s,
            None => break,
        };
        expect_token(state, Token::Colon)?;
        let ty = type_expr(state)?.ok_or(CompileError::Expected("type expr"))?;
        fields.insert(key, ty)?;

        if token(state, Token::Comma)?.is_none() {
            break;
        }
    }
    expect_token(state, Token::CurlyRight)?;

    Ok(Ty::struct_(fields))
}

// ### Statements

fn type_def_stmt(state: &mut State) -> Compile<()> {
    let tb = type_binding(state)?.ok_or(CompileError::Expected("type binding"))?;
    expect_token(state, Token::ColonEq)?;
    let te = type_expr(state)?.ok_or(CompileError::Expected("type expr"))?;
    state.ty_scope.assign(tb, te);
    Ok(())
}

fn assign_stmt(state: &mut State, rec: MemLocation) -> Compile<()> {
    expr(state)?.ok_or(CompileError::Expected("expr"))?;
    state.write(IR::Mov, rec.to_dest(), Src::PopStack, rec.ty);
    Ok(())
}

fn let_stmt(state: &mut State) -> Compile<()> {
    let b = binding(state)?.ok_or(CompileError::Expected("binding"))?;
    let bind_ty = if token(state, Token::Colon)?.is_some() {
        let ty = type_expr(state)?.ok_or(CompileError::Expected("type expr"))?;
        Some(ty)
    } else {
        None
    };
    expect_token(state, Token::ColonEq)?;
    let rec = expr(state)?.ok_or(CompileError::Expected("expr"))?;
    match bind_ty {
        Some(b) => b.check(&rec.ty)?,
        None => {}
    };
    state.scope.assign(b, rec);
    Ok(())
}

fn stmt(state: &mut State) -> CompileOpt<()> {
    match state.lexer.peek()? {
        Token::Let => {
            state.lexer.advance();
            let_stmt(state)?;
        }
        Token::Type => {
            state.lexer.advance();
            type_def_stmt(state)?;
        }
        Token::Identifier(_) => {
            let rec = lvalue(state)?.unwrap();
            if token(state, Token::ColonEq)?.is_some() {
                // x := y
                assign_stmt(state, rec)?;
            } else {
                // x + y
                let lhs = state.write(IR::Mov, Dest::Stack, rec.to_src(), rec.ty);
                op_expr(state, lhs)?;
            };
        }
        _ => {
            expr(state)?;
        }
    };
    Ok(Some(()))
}

#[allow(dead_code)]
fn program(input: &str) -> Compile<Vec<IR>> {
    let mut state = State::new(input);
    loop {
        if stmt(&mut state)?.is_none() {
            break;
        }
        if token(&mut state, Token::Semicolon)?.is_none() {
            break;
        }
    }
    expect_token(&mut state, Token::EndOfInput)?;
    Ok(state.output)
}

fn main() {
    println!("hello, world!");
}

#[cfg(test)]
mod test {
    use super::*;
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
            vec![IR::Mov(IRDest::PushStack, IRSrc::Immediate(123))],
        )
    }

    #[test]
    fn whitespace() {
        expect_ir(
            "  
            123
            
            ",
            vec![IR::Mov(IRDest::PushStack, IRSrc::Immediate(123))],
        )
    }

    #[test]
    fn comments() {
        expect_ir(
            "123 # This is a comment",
            vec![IR::Mov(IRDest::PushStack, IRSrc::Immediate(123))],
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
            vec![IR::Mov(IRDest::PushStack, IRSrc::Immediate(1))],
        );
        expect_ir(
            "false",
            vec![IR::Mov(IRDest::PushStack, IRSrc::Immediate(0))],
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
                IR::Mov(IRDest::PushStack, IRSrc::Immediate(123)),
                IR::Mov(IRDest::PushStack, IRSrc::Immediate(1)),
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
                IR::Mov(IRDest::PushStack, IRSrc::Immediate(1)),
                IR::Mov(IRDest::PushStack, IRSrc::Immediate(2)),
                IR::Mov(IRDest::PushStack, IRSrc::StackOffset(1)),
            ],
        )
    }

    #[test]
    fn type_exprs() {
        expect_ir(
            "
                let x : Int := 1;
            ",
            vec![IR::Mov(IRDest::PushStack, IRSrc::Immediate(1))],
        );
        expect_err(
            "
            let x : Bool := 1;
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
                IR::Mov(IRDest::PushStack, IRSrc::Immediate(1)),
                IR::Mov(IRDest::PushStack, IRSrc::Immediate(3)),
                IR::Mov(IRDest::PushStack, IRSrc::StackOffset(0)),
                IR::Mov(IRDest::StackOffset(1), IRSrc::PopStack),
                IR::Mov(IRDest::PushStack, IRSrc::StackOffset(1)),
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
                IR::Mov(IRDest::PushStack, IRSrc::Immediate(1)), //     [x: 1]
                IR::Mov(IRDest::PushStack, IRSrc::Immediate(2)), //     [y: 2, x: 1]
                IR::Mov(IRDest::PushStack, IRSrc::StackOffset(1)), //   [1, y: 2, x: 1]
                IR::Mov(IRDest::PushStack, IRSrc::Immediate(3)), //     [3, 1, y: 2, x: 1]
                IR::Add(IRDest::StackOffset(0), IRSrc::PopStack), //    [4, y: 2, x: 1]
                IR::Mov(IRDest::PushStack, IRSrc::StackOffset(1)), //   [2, 4, y: 2, x: 1]
                IR::Add(IRDest::StackOffset(0), IRSrc::PopStack), //    [6, y: 2, x: 1]
            ],
        )
    }

    #[test]
    fn typechecked_arithmetic() {
        expect_err(
            "1 + true",
            CompileError::ExpectedType(Ty::int(), Ty::bool()),
        );
        expect_err(
            "true + 1",
            CompileError::ExpectedType(Ty::int(), Ty::bool()),
        );
    }

    #[test]
    fn parens() {
        expect_ir(
            "3 * (4 + 5)",
            vec![
                IR::Mov(IRDest::PushStack, IRSrc::Immediate(3)),
                IR::Mov(IRDest::PushStack, IRSrc::Immediate(4)),
                IR::Mov(IRDest::PushStack, IRSrc::Immediate(5)),
                IR::Add(IRDest::StackOffset(0), IRSrc::PopStack),
                IR::Mult(IRDest::StackOffset(0), IRSrc::PopStack),
            ],
        )
    }

    #[test]
    fn precedence() {
        expect_ir(
            "4 + 5 * 3",
            vec![
                IR::Mov(IRDest::PushStack, IRSrc::Immediate(4)),
                IR::Mov(IRDest::PushStack, IRSrc::Immediate(5)),
                IR::Mov(IRDest::PushStack, IRSrc::Immediate(3)),
                IR::Mult(IRDest::StackOffset(0), IRSrc::PopStack),
                IR::Add(IRDest::StackOffset(0), IRSrc::PopStack),
            ],
        )
    }

    #[test]
    fn negation() {
        expect_ir(
            "-3",
            vec![
                IR::Mov(IRDest::PushStack, IRSrc::Immediate(0)),
                IR::Mov(IRDest::PushStack, IRSrc::Immediate(3)),
                IR::Sub(IRDest::StackOffset(0), IRSrc::PopStack),
            ],
        );
    }

    #[test]
    fn ref_deref() {
        expect_ir(
            "
            let x := 1;
            let ptr := &x;
            @(ptr)
        ",
            vec![
                IR::Mov(IRDest::PushStack, IRSrc::Immediate(1)),
                IR::LoadAddress(IRDest::PushStack, IRSrc::StackOffset(0)),
                // @(ptr)
                IR::Mov(IRDest::PushStack, IRSrc::StackOffset(0)),
                IR::Mov(IRDest::R0, IRSrc::PopStack),
                IR::Mov(IRDest::PushStack, IRSrc::R0Offset(0)),
            ],
        )
    }

    #[test]
    fn deref_lvalue() {
        expect_ir(
            "
            let x := 1;
            let ptr := &x;
            @ptr
        ",
            vec![
                IR::Mov(IRDest::PushStack, IRSrc::Immediate(1)),
                IR::LoadAddress(IRDest::PushStack, IRSrc::StackOffset(0)),
                IR::Mov(IRDest::R0, IRSrc::StackOffset(0)),
                IR::Mov(IRDest::PushStack, IRSrc::R0Offset(0)),
            ],
        )
    }

    #[test]
    fn type_casting() {
        expect_ir(
            "(true as Int) + 2",
            vec![
                IR::Mov(IRDest::PushStack, IRSrc::Immediate(1)),
                IR::Mov(IRDest::PushStack, IRSrc::Immediate(2)),
                IR::Add(IRDest::StackOffset(0), IRSrc::PopStack),
            ],
        )
    }

    #[test]
    fn type_alias() {
        expect_ir(
            "
                type Word := Int;
                let a : Word := 3    
            ",
            vec![IR::Mov(IRDest::PushStack, IRSrc::Immediate(3))],
        )
    }

    #[test]
    fn struct_() {
        expect_ir(
            "
            type Point := struct { x: Int, y: Int };
            let p := Point { x: 1, y: 2 };
            (p).x
        ",
            vec![
                // allocate
                IR::Sub(IRDest::SP, IRSrc::Immediate(2)),
                // initialize
                IR::Mov(IRDest::PushStack, IRSrc::Immediate(1)),
                IR::Mov(IRDest::StackOffset(0), IRSrc::PopStack),
                IR::Mov(IRDest::PushStack, IRSrc::Immediate(2)),
                IR::Mov(IRDest::StackOffset(1), IRSrc::PopStack),
                // put whole value on stack
                IR::Sub(IRDest::SP, IRSrc::Immediate(2)),
                IR::Mov(IRDest::StackOffset(0), IRSrc::StackOffset(2)),
                IR::Mov(IRDest::StackOffset(1), IRSrc::StackOffset(3)),
                // get field (leaving remainder on stack)
                IR::Mov(IRDest::PushStack, IRSrc::StackOffset(0)),
            ],
        );
    }

    #[test]
    fn struct_lvalue() {
        expect_ir(
            "
            type Point := struct { x: Int, y: Int };
            let p := Point { x: 1, y: 2 };
            p.y := 3;
            p.x
        ",
            vec![
                // allocate
                IR::Sub(IRDest::SP, IRSrc::Immediate(2)),
                // initialize
                IR::Mov(IRDest::PushStack, IRSrc::Immediate(1)),
                IR::Mov(IRDest::StackOffset(0), IRSrc::PopStack),
                IR::Mov(IRDest::PushStack, IRSrc::Immediate(2)),
                IR::Mov(IRDest::StackOffset(1), IRSrc::PopStack),
                // assign
                IR::Mov(IRDest::PushStack, IRSrc::Immediate(3)),
                IR::Mov(IRDest::StackOffset(1), IRSrc::PopStack),
                // get just the field
                IR::Mov(IRDest::PushStack, IRSrc::StackOffset(0)),
            ],
        );
    }

    #[test]
    fn struct_pointer() {
        expect_ir(
            "
            type Point := struct { x: Int, y: Int };
            let p := Point { x: 1, y: 2 };
            let ptr := &p.x;
            p.x := 5;
            @(ptr)
        ",
            vec![
                //  Point { x: 1, y: 2 };
                IR::Sub(IRDest::SP, IRSrc::Immediate(2)),
                IR::Mov(IRDest::PushStack, IRSrc::Immediate(1)),
                IR::Mov(IRDest::StackOffset(0), IRSrc::PopStack),
                IR::Mov(IRDest::PushStack, IRSrc::Immediate(2)),
                IR::Mov(IRDest::StackOffset(1), IRSrc::PopStack),
                // &p.x
                IR::LoadAddress(IRDest::PushStack, IRSrc::StackOffset(0)),
                // p.x := 5
                IR::Mov(IRDest::PushStack, IRSrc::Immediate(5)),
                IR::Mov(IRDest::StackOffset(1), IRSrc::PopStack),
                // @ptr
                IR::Mov(IRDest::PushStack, IRSrc::StackOffset(0)),
                IR::Mov(IRDest::R0, IRSrc::PopStack),
                IR::Mov(IRDest::PushStack, IRSrc::R0Offset(0)),
            ],
        )
    }

    #[test]
    fn struct_pointer_lvalue() {
        expect_ir(
            "
            type Point := struct { x: Int, y: Int };
            let p := Point { x: 1, y: 2 };
            let ptr := &p.x;
            @ptr
        ",
            vec![
                //  Point { x: 1, y: 2 };
                IR::Sub(IRDest::SP, IRSrc::Immediate(2)),
                IR::Mov(IRDest::PushStack, IRSrc::Immediate(1)),
                IR::Mov(IRDest::StackOffset(0), IRSrc::PopStack),
                IR::Mov(IRDest::PushStack, IRSrc::Immediate(2)),
                IR::Mov(IRDest::StackOffset(1), IRSrc::PopStack),
                // &p.x
                IR::LoadAddress(IRDest::PushStack, IRSrc::StackOffset(0)),
                // @ptr
                IR::Mov(IRDest::R0, IRSrc::StackOffset(0)),
                IR::Mov(IRDest::PushStack, IRSrc::R0Offset(0)),
            ],
        )
    }

    // TODO oneof / bitsets
}
