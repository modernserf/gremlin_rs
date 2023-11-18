use std::collections::HashMap;

// big list of optimizations I'm not doing yet
// - constant folding
// - registers
// - direct writes from expr to assign target
// TODO: fuse sequential [(push,x), (y,pop)] IR into [(y, x)]

pub type Word = i32;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum EA {
    Immediate(Word),
    // registers
    R0,
    SP,
    // registers with offset
    StackOffset(Word),
    R0Offset(Word),
    // (SP + R0 + offset)
    Indexed(Word),
    // stack ops
    PushStack,
    PopStack,
}

type IRDest = EA;
type IRSrc = EA;
type IROp = fn(IRDest, IRSrc) -> IR;

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
    fn get_src(&mut self, src: EA) -> Word {
        match src {
            EA::Immediate(value) => value,
            EA::R0 => self.r0,
            EA::R0Offset(offset) => self.memory[(self.r0 + offset) as usize],
            EA::StackOffset(offset) => self.memory[(self.sp + offset) as usize],
            EA::PopStack => {
                let value = self.memory[self.sp as usize];
                self.sp += 1;
                value
            }
            EA::Indexed(offset) => self.memory[(self.r0 + self.sp + offset) as usize],
            _ => unimplemented!(),
        }
    }
    fn get_effective_address(&self, src: EA) -> Word {
        match src {
            EA::StackOffset(offset) => self.sp + offset,
            _ => unimplemented!(),
        }
    }
    fn get_dest(&mut self, dest: EA) -> &mut Word {
        match dest {
            EA::R0 => &mut self.r0,
            EA::R0Offset(offset) => &mut self.memory[(self.r0 + offset) as usize],
            EA::SP => &mut self.sp,
            EA::StackOffset(offset) => &mut self.memory[(self.sp + offset) as usize],
            EA::PushStack => {
                self.sp -= 1;
                &mut self.memory[self.sp as usize]
            }
            _ => unimplemented!(),
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
use CompileError::*;

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
    Array,
    OneOf,
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
                'a' => {
                    self.adv_char();
                    match self.peek_char() {
                        'r' => self.keyword_idx("array", 1, Token::Array),
                        _ => self.keyword_idx("as", 1, Token::As),
                    }
                }
                's' => self.keyword("struct", Token::Struct),
                'o' => self.keyword("oneof", Token::OneOf),
                ';' => self.operator(Token::Semicolon),
                ',' => self.operator(Token::Comma),
                '.' => self.operator(Token::Dot),
                '+' => self.operator(Token::Plus),
                '-' => self.operator(Token::Minus),
                '*' => self.operator(Token::Star),
                '&' => self.operator(Token::Ampersand),
                '(' => self.operator(Token::ParLeft),
                ')' => self.operator(Token::ParRight),
                '{' => self.operator(Token::CurlyLeft),
                '}' => self.operator(Token::CurlyRight),
                '[' => self.operator(Token::SqLeft),
                ']' => self.operator(Token::SqRight),
                ':' => {
                    self.adv_char();
                    match self.peek_char() {
                        '=' => self.operator(Token::ColonEq),
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
    fn oneof(data: TyOneOf) -> Self {
        Self {
            kind: TyKind::OneOf(Box::new(data)),
            ref_level: 0,
        }
    }
    fn bitset(data: TyOneOf) -> Self {
        Self {
            kind: TyKind::BitSet(Box::new(data)),
            ref_level: 0,
        }
    }
    fn array(item_ty: Ty, capacity: Word) -> Self {
        Self {
            kind: TyKind::Array(Box::new(TyArray {
                ty: item_ty,
                capacity,
            })),
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
            Err(InvalidDeref)
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
            Err(ExpectedType(self.clone(), other.clone()))
        }
    }
    fn cast(&self, other: Self) -> Compile<Self> {
        if self.size() != other.size() {
            Err(InvalidCast)
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
            TyKind::OneOf(_) => 1,
            // TODO: large bitsets
            TyKind::BitSet(_) => 1,
            TyKind::Array(a) => a.ty.size() * a.capacity,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum TyKind {
    Int,
    Bool,
    Struct(Box<TyStruct>),
    OneOf(Box<TyOneOf>),
    BitSet(Box<TyOneOf>),
    Array(Box<TyArray>),
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
            return Err(DuplicateField);
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
        self.fields.get(k).ok_or(MissingField)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct StructField {
    ty: Ty,
    offset: Word,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct TyOneOfMember {
    index: Word,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct TyOneOf {
    id: usize,
    members: HashMap<String, TyOneOfMember>,
}

impl TyOneOf {
    fn new(id: usize) -> Self {
        Self {
            id,
            members: HashMap::new(),
        }
    }
    fn insert(&mut self, key: String, index: Word) -> Compile<()> {
        match self.members.insert(key, TyOneOfMember { index }) {
            Some(_) => Err(DuplicateField),
            None => Ok(()),
        }
    }
    fn get(&self, key: &str) -> Compile<&TyOneOfMember> {
        self.members.get(key).ok_or(MissingField)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct TyArray {
    ty: Ty,
    capacity: Word,
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
        Dest::Mem(self.kind)
    }
    fn to_src(&self) -> Src {
        match self.kind {
            MemLocationKind::FrameOffset(offset) => Src::FrameOffset(offset),
            _ => unimplemented!(),
        }
    }
    fn pop(self, state: &State) -> Compile<Src> {
        match self.kind {
            MemLocationKind::FrameOffset(frame_offset) => {
                let stack_offset = state.current_frame_size - frame_offset;
                if stack_offset == 0 {
                    return Ok(Src::PopStack);
                }
                unimplemented!()
            }
            _ => unimplemented!(),
        }
    }
    fn cast(self, ty: Ty) -> Compile<Self> {
        Ok(Self {
            kind: self.kind,
            ty: self.ty.cast(ty)?,
        })
    }
    fn struct_field(self, field: &StructField) -> MemLocation {
        match self.kind {
            MemLocationKind::FrameOffset(parent_offset) => {
                MemLocation::local(parent_offset - field.offset, field.ty.clone())
            }
            _ => unimplemented!(),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum MemLocationKind {
    FrameOffset(Word),
    R0,
    R0Offset(Word),
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
            .ok_or_else(|| UnknownIdentifier(key.to_string()))
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
            .ok_or_else(|| UnknownTypeIdentifier(key.to_string()))
    }
    fn assign(&mut self, key: String, record: Ty) {
        self.data.insert(key, record);
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Dest {
    Stack,
    Mem(MemLocationKind),
    // FrameOffset(Word),
    // R0,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Src {
    Immediate(Word),
    FrameOffset(Word),
    PopStack,
    R0,
    R0Offset(Word),
    Indexed(Word),
}

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
    fn write(&mut self, op: IROp, res: MemLocation, src: Src) -> MemLocation {
        if res.ty.size() == 1 {
            return self.write_1(op, res.to_dest(), src, res.ty, 0);
        }

        for i in 0..res.ty.size() {
            self.write_1(op, res.to_dest(), src, res.ty.clone(), i);
        }

        res
    }
    fn push(&mut self, src: Src, dest_ty: Ty) -> MemLocation {
        if dest_ty.size() == 1 {
            return self.write_1(IR::Mov, Dest::Stack, src, dest_ty, 0);
        }
        let loc = self.allocate(&dest_ty);
        return self.write(IR::Mov, loc, src);
    }
    fn write_1(&mut self, op: IROp, dest: Dest, src: Src, dest_ty: Ty, i: Word) -> MemLocation {
        let ir_src = match src {
            Src::Immediate(value) => EA::Immediate(value),
            Src::FrameOffset(offset) => {
                let stack_offset = self.current_frame_size - offset + i;
                EA::StackOffset(stack_offset)
            }
            Src::PopStack => {
                self.current_frame_size -= 1;
                EA::PopStack
            }
            Src::R0Offset(offset) => EA::R0Offset(offset),
            Src::R0 => EA::R0,
            Src::Indexed(offset) => EA::Indexed(offset),
        };
        let (ir_dest, result) = match dest {
            Dest::Stack => {
                self.current_frame_size += 1;
                (
                    EA::PushStack,
                    MemLocation::local(self.current_frame_size, dest_ty),
                )
            }
            Dest::Mem(mem) => {
                let ir_dest = match mem {
                    MemLocationKind::FrameOffset(offset) => {
                        let stack_offset = self.current_frame_size - offset + i;
                        EA::StackOffset(stack_offset)
                    }
                    MemLocationKind::R0 => EA::R0,
                    MemLocationKind::R0Offset(offset) => EA::R0Offset(offset),
                };
                (
                    ir_dest,
                    MemLocation {
                        kind: mem,
                        ty: dest_ty,
                    },
                )
            }
        };
        self.output.push(op(ir_dest, ir_src));
        result
    }
    fn allocate(&mut self, ty: &Ty) -> MemLocation {
        self.current_frame_size += ty.size();
        self.output.push(IR::Sub(EA::SP, EA::Immediate(ty.size())));
        MemLocation::local(self.current_frame_size, ty.clone())
    }
}

struct OpParser {
    op_stack: Vec<Op>,
    operands: Vec<Expr>,
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
    fn new(left: Expr) -> Self {
        Self {
            op_stack: Vec::new(),
            operands: vec![left],
        }
    }
    fn apply(&mut self, state: &mut State, op: Op) -> Compile<()> {
        // TODO: constant folding
        let right_expr = self.operands.pop().expect("rhs");
        let mut left = self.operands.pop().expect("lhs").resolve(state);
        let right = right_expr.resolve(state);
        let dest_ty = op.check_ty(left.ty.clone(), right.ty)?;
        left.ty = dest_ty;
        let res = state.write(op.ir(), left, Src::PopStack);
        self.operands.push(Expr::Resolved(res));
        Ok(())
    }
    fn unwind(&mut self, state: &mut State) -> Compile<Expr> {
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
    fn push_rhs(&mut self, rhs: Expr) {
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
    token(state, tok.clone())?.ok_or(ExpectedToken(tok))
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

fn type_ident_token(state: &mut State) -> CompileOpt<String> {
    match state.lexer.peek()? {
        Token::TypeIdentifier(s) => {
            state.lexer.advance();
            Ok(Some(s))
        }
        _ => Ok(None),
    }
}

// ### Exprs

#[derive(Debug)]
enum Expr {
    Constant(Word, Ty),
    LValue(MemLocation),
    DerefLValue(MemLocation, StructField),
    Resolved(MemLocation),
}

impl Expr {
    fn resolve(self, state: &mut State) -> MemLocation {
        match self {
            Expr::Constant(value, ty) => state.push(Src::Immediate(value), ty),
            Expr::Resolved(loc) => loc,
            Expr::LValue(loc) => state.push(loc.to_src(), loc.ty),
            Expr::DerefLValue(loc, field) => {
                state.write(IR::Mov, MemLocation::r0(loc.ty.clone()), loc.to_src());
                state.push(Src::R0Offset(field.offset), field.ty)
            }
        }
    }
    fn assign(self, state: &mut State, value: Expr) -> Compile<Expr> {
        match self {
            Expr::LValue(target) => {
                let src = value.resolve(state);
                let out = state.write(IR::Mov, target, src.pop(state)?);
                Ok(Expr::Resolved(out))
            }
            Expr::DerefLValue(target, field) => {
                let src = value.resolve(state);
                state.write(
                    IR::Mov,
                    MemLocation::r0(target.ty.add_ref()),
                    target.to_src(),
                );
                Ok(Expr::Resolved(state.write(
                    IR::Mov,
                    MemLocation {
                        ty: field.ty,
                        kind: MemLocationKind::R0Offset(field.offset),
                    },
                    src.pop(state)?,
                )))
            }
            _ => return Err(Expected("lvalue")),
        }
    }
    fn ty_(&self) -> &Ty {
        match self {
            Expr::Constant(_, ty) => ty,
            Expr::LValue(m) => &m.ty,
            Expr::DerefLValue(m, _) => &m.ty,
            Expr::Resolved(m) => &m.ty,
        }
    }
    fn item_ty(&self) -> Compile<&Ty> {
        match &self.ty_().kind {
            TyKind::Array(a) => Ok(&a.ty),
            _ => Err(Expected("array")),
        }
    }
    fn struct_field(self, field_name: &str) -> Compile<Expr> {
        let ty = self.ty_();
        if ty.ref_level > 1 {}

        let fields = match &self.ty_().kind {
            TyKind::Struct(fs) => fs,
            _ => return Err(Expected("struct")),
        };
        let field = fields.get(field_name)?.clone();

        match self {
            Expr::Constant(_, _) => unreachable!(),
            Expr::LValue(m) => Ok(Expr::LValue(m.struct_field(&field))),
            Expr::DerefLValue(m, parent) => Ok(Expr::DerefLValue(
                m,
                StructField {
                    ty: field.ty,
                    offset: parent.offset + field.offset,
                },
            )),
            Expr::Resolved(m) => Ok(Expr::LValue(m.struct_field(&field))),
        }
    }
    fn bitset_field(&self, field_name: &str) -> Compile<TyOneOfMember> {
        let fields = match &self.ty_().kind {
            TyKind::BitSet(fs) => fs,
            _ => return Err(Expected("bitset")),
        };
        Ok(fields.get(field_name)?.clone())
    }

    fn add_ref(self, state: &mut State) -> Compile<Expr> {
        match self {
            Expr::LValue(mem) => {
                let dest_ty = mem.ty.add_ref();
                let out = state.write_1(IR::LoadAddress, Dest::Stack, mem.to_src(), dest_ty, 0);
                Ok(Expr::Resolved(out))
            }
            _ => Err(Expected("lvalue")),
        }
    }
    fn deref(self, state: &mut State) -> Compile<Expr> {
        match self {
            Expr::Resolved(mem) => {
                let res = state.write(IR::Mov, MemLocation::r0(mem.ty.clone()), mem.pop(state)?);
                Ok(Expr::Resolved(
                    state.push(Src::R0Offset(0), res.ty.deref()?),
                ))
            }
            Expr::LValue(mem) => Ok(Expr::DerefLValue(
                mem.clone(),
                StructField {
                    ty: mem.ty,
                    offset: 0,
                },
            )),
            _ => unimplemented!(),
        }
    }
    fn cast_ty(self, cast_ty: Ty) -> Compile<Expr> {
        Ok(match self {
            Expr::Constant(x, ty) => Expr::Constant(x, ty.cast(cast_ty)?),
            Expr::LValue(mem) => Expr::LValue(mem.cast(cast_ty)?),
            Expr::DerefLValue(_, _) => unimplemented!(),
            Expr::Resolved(mem) => Expr::Resolved(mem.cast(cast_ty)?),
        })
    }
}

fn bitset_expr(state: &mut State, fields: &TyOneOf) -> Compile<Expr> {
    let ty = Ty::bitset(fields.clone());
    let mut value = 0;
    expect_token(state, Token::CurlyLeft)?;
    loop {
        let key = match type_ident_token(state)? {
            Some(key) => key,
            _ => break,
        };
        let field = fields.get(&key)?;
        // TODO: check for dupes
        value |= 1 << field.index;

        if token(state, Token::Comma)?.is_none() {
            break;
        }
    }
    expect_token(state, Token::CurlyRight)?;
    Ok(Expr::Constant(value, ty))
}

fn struct_expr(state: &mut State, fields: &TyStruct) -> Compile<Expr> {
    let base_res: MemLocation = state.allocate(&Ty::struct_(fields.clone()));
    expect_token(state, Token::CurlyLeft)?;
    loop {
        let field_name = match ident_token(state)? {
            Some(s) => s,
            None => break,
        };
        expect_token(state, Token::Colon)?;
        let field = fields.get(&field_name)?;
        let field_loc = base_res.clone().struct_field(field);
        let value = expr(state)?.ok_or(Expected("expr"))?;
        field.ty.check(&value.ty)?;
        state.write(IR::Mov, field_loc, Src::PopStack);

        if token(state, Token::Comma)?.is_none() {
            break;
        }
    }
    expect_token(state, Token::CurlyRight)?;
    Ok(Expr::Resolved(base_res))
}

fn oneof_member_expr(state: &mut State, name: &str) -> Compile<Expr> {
    let ty = state.ty_scope.get(name)?;
    let fields = match &ty.kind {
        TyKind::OneOf(fields) => fields,
        _ => return Err(Expected("oneof")),
    };
    expect_token(state, Token::Dot)?;
    let member_name = match type_ident_token(state)? {
        Some(key) => key,
        _ => return Err(Expected("oneof member")),
    };
    let field = fields.get(&member_name)?;
    Ok(Expr::Constant(field.index, ty))
}

fn base_expr(state: &mut State) -> CompileOpt<Expr> {
    let res = match state.lexer.peek()? {
        Token::ParLeft => {
            state.lexer.advance();
            let res = expr(state)?.ok_or(Expected("expr"))?;
            expect_token(state, Token::ParRight)?;
            Expr::Resolved(res)
        }
        Token::TypeIdentifier(name) => {
            state.lexer.advance();
            match state.lexer.peek()? {
                Token::Dot => oneof_member_expr(state, &name)?,
                _ => {
                    let ty = state.ty_scope.get(&name)?;
                    match &ty.kind {
                        TyKind::Struct(fields) => struct_expr(state, fields)?,
                        TyKind::OneOf(fields) => bitset_expr(state, fields)?,
                        _ => return Err(Expected("struct")),
                    }
                }
            }
        }
        Token::Integer(int) => {
            state.lexer.advance();
            Expr::Constant(int, Ty::int())
        }
        Token::True => {
            state.lexer.advance();
            Expr::Constant(1, Ty::bool())
        }
        Token::False => {
            state.lexer.advance();
            Expr::Constant(0, Ty::bool())
        }
        Token::Identifier(name) => {
            state.lexer.advance();
            let record = state.scope.get(&name)?;
            Expr::LValue(record)
        }
        Token::Array => {
            state.lexer.advance();
            expect_token(state, Token::SqLeft)?;
            let item_ty = type_expr(state)?.ok_or(Expected("type expr"))?;
            expect_token(state, Token::SqRight)?;
            expect_token(state, Token::CurlyLeft)?;
            let mut capacity = 0;
            let frame_offset = state.current_frame_size;
            loop {
                // TODO: need to keep stack clean between exprs
                match expr(state)? {
                    Some(mem) => {
                        item_ty.check(&mem.ty)?;
                        capacity += 1;
                    }
                    None => break,
                };
                if token(state, Token::Comma)?.is_none() {
                    break;
                };
            }

            expect_token(state, Token::CurlyRight)?;
            let ty = Ty::array(item_ty, capacity);
            Expr::Resolved(MemLocation::local(frame_offset, ty))
        }
        _ => return Ok(None),
    };
    Ok(Some(res))
}

fn postfix_expr(state: &mut State) -> CompileOpt<Expr> {
    let mut left = match base_expr(state)? {
        Some(e) => e,
        None => return Ok(None),
    };
    loop {
        match state.lexer.peek()? {
            Token::SqLeft => {
                state.lexer.advance();
                match state.lexer.peek()? {
                    Token::SqRight => {
                        state.lexer.advance();
                        left = left.deref(state)?
                    }
                    _ => {
                        let index = expr(state)?.ok_or(Expected("expr"))?;
                        expect_token(state, Token::SqRight)?;
                        state.write(
                            IR::Mov,
                            MemLocation::r0(index.ty.clone()),
                            index.pop(state)?,
                        );
                        let item_ty = left.item_ty()?.clone();
                        left = Expr::Resolved(state.push(Src::Indexed(0), item_ty));
                    }
                }
            }
            Token::Dot => {
                state.lexer.advance();
                match state.lexer.peek()? {
                    Token::Identifier(field_name) => {
                        state.lexer.advance();
                        let next = left.struct_field(&field_name)?;
                        left = next
                    }
                    Token::TypeIdentifier(field_name) => {
                        state.lexer.advance();
                        let field = left.bitset_field(&field_name)?;
                        let left_res = left.resolve(state);
                        state.write(IR::BitTest, left_res, Src::Immediate(field.index));
                        left = Expr::Resolved(state.push(Src::R0, Ty::bool()));
                    }
                    _ => return Err(Expected("field")),
                }
            }
            _ => return Ok(Some(left)),
        };
    }
}

fn unary_op_expr(state: &mut State) -> CompileOpt<Expr> {
    let out = match state.lexer.peek()? {
        Token::Minus => {
            // TODO: NEG IR, constant folding
            state.lexer.advance();
            let tgt = state.push(Src::Immediate(0), Ty::int());
            let operand = unary_op_expr(state)?
                .ok_or(Expected("expr"))?
                .resolve(state);
            let out = state.write(IR::Sub, tgt, operand.pop(state)?);
            Expr::Resolved(out)
        }
        Token::Ampersand => {
            state.lexer.advance();
            let operand = unary_op_expr(state)?.ok_or(Expected("expr"))?;
            operand.add_ref(state)?
        }
        _ => return postfix_expr(state),
    };
    Ok(Some(out))
}

fn op_expr(state: &mut State) -> CompileOpt<Expr> {
    let left = match unary_op_expr(state)? {
        Some(expr) => expr,
        None => return Ok(None),
    };
    let mut op_parser = OpParser::new(left);

    loop {
        match operator(state)? {
            Some(op) => {
                op_parser.next(state, op)?;
                let res = unary_op_expr(state)?.ok_or(Expected("expr"))?;
                op_parser.push_rhs(res);
            }
            None => return Ok(Some(op_parser.unwind(state)?)),
        }
    }
}

fn assign_expr(state: &mut State) -> CompileOpt<Expr> {
    let left = match op_expr(state)? {
        Some(x) => x,
        None => return Ok(None),
    };
    if token(state, Token::ColonEq)?.is_none() {
        return Ok(Some(left));
    }
    let expr = op_expr(state)?.ok_or(Expected("expr"))?;
    left.assign(state, expr).map(Some)
}

fn as_expr(state: &mut State) -> CompileOpt<Expr> {
    let value = match assign_expr(state)? {
        Some(x) => x,
        None => return Ok(None),
    };
    if token(state, Token::As)?.is_none() {
        return Ok(Some(value));
    }
    let ty = type_expr(state)?.ok_or(Expected("type expr"))?;
    Ok(Some(value.cast_ty(ty)?))
}

fn expr(state: &mut State) -> CompileOpt<MemLocation> {
    Ok(as_expr(state)?.map(|e| e.resolve(state)))
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

fn type_binding(state: &mut State) -> CompileOpt<String> {
    match state.lexer.peek()? {
        Token::TypeIdentifier(str) => {
            state.lexer.advance();
            Ok(Some(str))
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
        let ty = type_expr(state)?.ok_or(Expected("type expr"))?;
        fields.insert(key, ty)?;

        if token(state, Token::Comma)?.is_none() {
            break;
        }
    }
    expect_token(state, Token::CurlyRight)?;

    Ok(Ty::struct_(fields))
}

fn oneof_ty(state: &mut State) -> Compile<Ty> {
    let mut data = TyOneOf::new(state.new_type_id());

    expect_token(state, Token::CurlyLeft)?;
    let mut i = 0;
    loop {
        let key = match type_ident_token(state)? {
            Some(key) => key,
            _ => break,
        };

        // TODO: allow setting numeric values for oneof members
        // `oneof {Jan := 1, Feb, Mar, Apr}`
        // enforce that numbers are increasing order

        data.insert(key, i)?;
        i += 1;

        if token(state, Token::Comma)?.is_none() {
            break;
        }
    }
    expect_token(state, Token::CurlyRight)?;

    Ok(Ty::oneof(data))
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
        Token::OneOf => {
            state.lexer.advance();
            oneof_ty(state).map(Some)
        }
        _ => Ok(None),
    }
}

// ### Statements

fn type_def_stmt(state: &mut State) -> Compile<()> {
    let tb = type_binding(state)?.ok_or(Expected("type binding"))?;
    expect_token(state, Token::ColonEq)?;
    let te = type_expr(state)?.ok_or(Expected("type expr"))?;
    state.ty_scope.assign(tb, te);
    Ok(())
}

fn let_stmt(state: &mut State) -> Compile<()> {
    let b = binding(state)?.ok_or(Expected("binding"))?;
    let bind_ty = if token(state, Token::Colon)?.is_some() {
        let ty = type_expr(state)?.ok_or(Expected("type expr"))?;
        Some(ty)
    } else {
        None
    };
    expect_token(state, Token::ColonEq)?;
    let rec = expr(state)?.ok_or(Expected("expr"))?;
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
    use super::{EA::*, IR::*};

    fn expect_ir(code: &str, ir: Vec<IR>) {
        assert_eq!(program(code), Ok(ir));
    }
    fn expect_result(code: &str, value: Word) {
        let ir = program(code).expect("compile");
        let res = Runtime::eval(&ir);
        assert_eq!(res, value);
    }
    fn expect_ir_result(code: &str, ir: Vec<IR>, result: Word) {
        let actual_ir = program(code).expect("compile");
        let actual_result: i32 = Runtime::eval(&actual_ir);
        assert_eq!(actual_ir, ir);
        assert_eq!(actual_result, result);
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
        expect_ir_result("123", vec![Mov(PushStack, Immediate(123))], 123);
    }

    #[test]
    fn whitespace() {
        expect_ir(
            "  
            123
            
            ",
            vec![Mov(PushStack, Immediate(123))],
        )
    }

    #[test]
    fn comments() {
        expect_ir(
            "123 # This is a comment",
            vec![Mov(PushStack, Immediate(123))],
        )
    }

    #[test]
    fn unexpected_char() {
        expect_err(" £ ", UnexpectedChar)
    }

    #[test]
    fn bools() {
        expect_ir("true", vec![Mov(PushStack, Immediate(1))]);
        expect_ir("false", vec![Mov(PushStack, Immediate(0))]);
    }

    #[test]
    fn sequences() {
        expect_ir(
            "
            123;
            true
        ",
            vec![Mov(PushStack, Immediate(123)), Mov(PushStack, Immediate(1))],
        )
    }

    #[test]
    fn let_stmts() {
        expect_ir_result(
            "
                let x := 1;
                let y := 2;
                x
            ",
            vec![
                Mov(PushStack, Immediate(1)),
                Mov(PushStack, Immediate(2)),
                Mov(PushStack, StackOffset(1)),
            ],
            1,
        );
    }

    #[test]
    fn type_exprs() {
        expect_ir(
            "
                let x : Int := 1;
            ",
            vec![Mov(PushStack, Immediate(1))],
        );
        expect_err(
            "
            let x : Bool := 1;
        ",
            ExpectedType(Ty::bool(), Ty::int()),
        )
    }

    #[test]
    fn assignment() {
        expect_ir_result(
            "
            let x := 1;
            let y := 3;
            x := y;
            x
        ",
            vec![
                Mov(PushStack, Immediate(1)),
                Mov(PushStack, Immediate(3)),
                Mov(PushStack, StackOffset(0)),
                Mov(StackOffset(1), PopStack),
                Mov(PushStack, StackOffset(1)),
            ],
            3,
        );
    }

    #[test]
    fn addition() {
        expect_ir_result(
            "
            let x := 1;
            let y := 2;
            x + 3 + y 
        ",
            vec![
                Mov(PushStack, Immediate(1)),   // [x: 1]
                Mov(PushStack, Immediate(2)),   // [y: 2, x: 1]
                Mov(PushStack, StackOffset(1)), // [1, y: 2, x: 1]
                Mov(PushStack, Immediate(3)),   // [3, 1, y: 2, x: 1]
                Add(StackOffset(0), PopStack),  // [4, y: 2, x: 1]
                Mov(PushStack, StackOffset(1)), // [2, 4, y: 2, x: 1]
                Add(StackOffset(0), PopStack),  // [6, y: 2, x: 1]
            ],
            6,
        );
    }

    #[test]
    fn typechecked_arithmetic() {
        expect_err("1 + true", ExpectedType(Ty::int(), Ty::bool()));
        expect_err("true + 1", ExpectedType(Ty::int(), Ty::bool()));
    }

    #[test]
    fn parens() {
        expect_ir(
            "(3) * (4 + 5)",
            vec![
                Mov(PushStack, Immediate(3)),
                Mov(PushStack, Immediate(4)),
                Mov(PushStack, Immediate(5)),
                Add(StackOffset(0), PopStack),
                Mult(StackOffset(0), PopStack),
            ],
        )
    }

    #[test]
    fn precedence() {
        expect_ir_result(
            "(4) + (5) * (3)",
            vec![
                Mov(PushStack, Immediate(4)),
                Mov(PushStack, Immediate(5)),
                Mov(PushStack, Immediate(3)),
                Mult(StackOffset(0), PopStack),
                Add(StackOffset(0), PopStack),
            ],
            19,
        );
    }

    #[test]
    fn negation() {
        expect_ir_result(
            "-3",
            vec![
                Mov(PushStack, Immediate(0)),
                Mov(PushStack, Immediate(3)),
                Sub(StackOffset(0), PopStack),
            ],
            -3,
        );
    }

    #[test]
    fn ref_deref() {
        expect_ir_result(
            "
            let x := 1;
            let ptr := &x;
            (ptr)[]
        ",
            vec![
                Mov(PushStack, Immediate(1)),
                LoadAddress(PushStack, StackOffset(0)),
                // (ptr)[]
                Mov(PushStack, StackOffset(0)),
                Mov(R0, PopStack),
                Mov(PushStack, R0Offset(0)),
            ],
            1,
        );
    }

    #[test]
    fn deref_lvalue() {
        expect_ir_result(
            "
            let x := 1;
            let ptr := &x;
            ptr[]
        ",
            vec![
                Mov(PushStack, Immediate(1)),
                LoadAddress(PushStack, StackOffset(0)),
                Mov(R0, StackOffset(0)),
                Mov(PushStack, R0Offset(0)),
            ],
            1,
        );
    }

    #[test]
    fn ptr_assign() {
        expect_ir_result(
            "
            let x := 1;
            let ptr := &x;
            ptr[] := 2;
            x
        ",
            vec![
                Mov(PushStack, Immediate(1)),
                LoadAddress(PushStack, StackOffset(0)),
                Mov(PushStack, Immediate(2)),
                Mov(R0, StackOffset(1)),
                Mov(R0Offset(0), PopStack),
                Mov(PushStack, StackOffset(1)),
            ],
            2,
        );
    }

    #[test]
    fn type_casting() {
        expect_ir(
            "(true as Int) + 2",
            vec![
                Mov(PushStack, Immediate(1)),
                Mov(PushStack, Immediate(2)),
                Add(StackOffset(0), PopStack),
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
            vec![Mov(PushStack, Immediate(3))],
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
                Sub(SP, Immediate(2)),
                // initialize
                Mov(PushStack, Immediate(1)),
                Mov(StackOffset(0), PopStack),
                Mov(PushStack, Immediate(2)),
                Mov(StackOffset(1), PopStack),
                // put whole value on stack
                Sub(SP, Immediate(2)),
                Mov(StackOffset(0), StackOffset(2)),
                Mov(StackOffset(1), StackOffset(3)),
                // get field (leaving remainder on stack)
                Mov(PushStack, StackOffset(0)),
            ],
        );
    }

    #[test]
    fn struct_lvalue() {
        expect_ir_result(
            "
            type Point := struct { x: Int, y: Int };
            let p := Point { x: 1, y: 2 };
            p.y := 3;
            p.x
        ",
            vec![
                // allocate
                Sub(SP, Immediate(2)),
                // initialize
                Mov(PushStack, Immediate(1)),
                Mov(StackOffset(0), PopStack),
                Mov(PushStack, Immediate(2)),
                Mov(StackOffset(1), PopStack),
                // assign
                Mov(PushStack, Immediate(3)),
                Mov(StackOffset(1), PopStack),
                // get just the field
                Mov(PushStack, StackOffset(0)),
            ],
            1,
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
            (ptr)[]
        ",
            vec![
                //  Point { x: 1, y: 2 };
                Sub(SP, Immediate(2)),
                Mov(PushStack, Immediate(1)),
                Mov(StackOffset(0), PopStack),
                Mov(PushStack, Immediate(2)),
                Mov(StackOffset(1), PopStack),
                // &p.x
                LoadAddress(PushStack, StackOffset(0)),
                // p.x := 5
                Mov(PushStack, Immediate(5)),
                Mov(StackOffset(1), PopStack),
                // ptr[]
                Mov(PushStack, StackOffset(0)),
                Mov(R0, PopStack),
                Mov(PushStack, R0Offset(0)),
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
            ptr[]
        ",
            vec![
                //  Point { x: 1, y: 2 };
                Sub(SP, Immediate(2)),
                Mov(PushStack, Immediate(1)),
                Mov(StackOffset(0), PopStack),
                Mov(PushStack, Immediate(2)),
                Mov(StackOffset(1), PopStack),
                // &p.x
                LoadAddress(PushStack, StackOffset(0)),
                // ptr[]
                Mov(R0, StackOffset(0)),
                Mov(PushStack, R0Offset(0)),
            ],
        )
    }

    #[test]
    fn struct_pointer_field() {
        expect_ir_result(
            "
            type Point := struct { x: Int, y: Int };
            let p := Point { x: 1, y: 2 };
            let ptr := &p;
            ptr[].y
        ",
            vec![
                //  Point { x: 1, y: 2 };
                Sub(SP, Immediate(2)),
                Mov(PushStack, Immediate(1)),
                Mov(StackOffset(0), PopStack),
                Mov(PushStack, Immediate(2)),
                Mov(StackOffset(1), PopStack),
                // &p
                LoadAddress(PushStack, StackOffset(0)),
                // ptr[].y
                Mov(R0, StackOffset(0)),
                Mov(PushStack, R0Offset(1)),
            ],
            2,
        );
    }

    #[test]
    fn oneof() {
        expect_ir(
            "
            type TrafficLight := oneof {Red, Yellow, Green};
            TrafficLight.Yellow
        ",
            vec![Mov(PushStack, Immediate(1))],
        )
    }

    #[test]
    fn bitset() {
        expect_ir_result(
            "
            type Flags := oneof {Carry, Overflow, Zero, Negative, Extend};
            let flags := Flags{Carry, Zero};
            flags.Carry
        ",
            vec![
                Mov(PushStack, Immediate(0b101)),
                Mov(PushStack, StackOffset(0)),
                BitTest(StackOffset(0), Immediate(0)),
                Mov(PushStack, R0),
            ],
            1,
        );
    }

    #[test]
    fn array() {
        expect_ir_result(
            "
                let xs := array[Int]{10, 20, 30};
                xs[1]
            ",
            vec![
                Mov(PushStack, Immediate(10)),
                Mov(PushStack, Immediate(20)),
                Mov(PushStack, Immediate(30)),
                Mov(PushStack, Immediate(1)),
                Mov(R0, PopStack),
                Mov(PushStack, Indexed(0)),
            ],
            20,
        );
    }
}
