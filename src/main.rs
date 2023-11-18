mod lexer;
mod runtime;
mod ty;
mod writer;

use lexer::*;
use runtime::*;
use std::collections::HashMap;
use ty::*;
use writer::*;

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

pub type Compile<T> = Result<T, CompileError>;
pub type CompileOpt<T> = Result<Option<T>, CompileError>;

type ScopeRecord = (Ty, MemLocation);
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
            .ok_or_else(|| UnknownIdentifier(key.to_string()))
    }
    fn assign(&mut self, key: String, loc: MemLocation, ty: Ty) {
        self.data.insert(key, (ty, loc));
    }
}

struct TyScope {
    data: HashMap<String, Ty>,
    next_type_id: usize,
}

impl TyScope {
    fn new() -> Self {
        Self {
            data: HashMap::from_iter(
                vec![("Int", Ty::int()), ("Bool", Ty::bool())]
                    .into_iter()
                    .map(|(k, v)| (k.to_string(), v)),
            ),
            next_type_id: 1,
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
    fn new_type_id(&mut self) -> usize {
        let id = self.next_type_id;
        self.next_type_id += 1;
        id
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
    fn apply(&mut self, writer: &mut Writer, op: Op) -> Compile<()> {
        let right_expr = self.operands.pop().expect("rhs");
        let left_expr = self.operands.pop().expect("lhs");
        let ty = op.check_ty(&left_expr.ty, &right_expr.ty)?;

        let result = match (left_expr.get_constant(), right_expr.get_constant()) {
            (Some(left), Some(right)) => Expr::constant(ty, op.inline(left, right)),
            _ => {
                let left = left_expr.resolve(writer);
                let right = right_expr.resolve(writer);
                writer.take_from(op.ir(), &left.loc, right.loc);
                Expr::resolved(ty, left.loc)
            }
        };

        self.operands.push(result);
        Ok(())
    }
    fn unwind(&mut self, writer: &mut Writer) -> Compile<Expr> {
        while let Some(op) = self.op_stack.pop() {
            self.apply(writer, op)?;
        }
        Ok(self.operands.pop().expect("op result"))
    }
    fn next(&mut self, writer: &mut Writer, op: Op) -> Compile<()> {
        match self.op_stack.pop() {
            Some(last_op) => {
                if last_op.precedence() > op.precedence() {
                    self.op_stack.push(last_op);
                    self.op_stack.push(op);
                } else {
                    self.apply(writer, last_op)?;
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
pub enum Op {
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
    fn check_ty(&self, left: &Ty, right: &Ty) -> Compile<Ty> {
        match self {
            Op::Add => self.arithmetic(left, right),
            Op::Sub => self.arithmetic(left, right),
            Op::Mul => self.arithmetic(left, right),
        }
    }
    fn arithmetic(&self, left: &Ty, right: &Ty) -> Compile<Ty> {
        Ty::int().check(left)?;
        Ty::int().check(right)?;
        Ok(Ty::int())
    }
    fn inline(&self, left: Word, right: Word) -> Word {
        match self {
            Op::Add => left + right,
            Op::Mul => left * right,
            Op::Sub => left - right,
        }
    }
    fn ir(&self) -> IROp {
        match self {
            Op::Add => IR::Add,
            Op::Sub => IR::Sub,
            Op::Mul => IR::Mult,
        }
    }
}

struct ResolvedExpr {
    loc: MemLocation,
    ty: Ty,
}

impl ResolvedExpr {
    fn new(ty: Ty, loc: MemLocation) -> Self {
        Self { ty, loc }
    }
}

#[derive(Debug)]
struct Expr {
    kind: ExprKind,
    ty: Ty,
}

#[derive(Debug)]
enum ExprKind {
    Constant(Word),
    LValue(MemLocation),
    DerefLValue(MemLocation, StructField),
    Resolved(MemLocation),
}

impl Expr {
    fn constant(ty: Ty, value: Word) -> Self {
        Self {
            ty,
            kind: ExprKind::Constant(value),
        }
    }
    fn lvalue(ty: Ty, loc: MemLocation) -> Self {
        Self {
            ty,
            kind: ExprKind::LValue(loc),
        }
    }
    fn deref_lvalue(ty: Ty, loc: MemLocation, struct_field: StructField) -> Self {
        Self {
            ty,
            kind: ExprKind::DerefLValue(loc, struct_field),
        }
    }
    fn resolved(ty: Ty, loc: MemLocation) -> Self {
        Self {
            ty,
            kind: ExprKind::Resolved(loc),
        }
    }
    fn resolve(self, writer: &mut Writer) -> ResolvedExpr {
        let loc = match self.kind {
            ExprKind::Constant(value) => writer.to_stack(IR::Mov, Src::Immediate(value), &self.ty),
            ExprKind::Resolved(loc) => loc,
            ExprKind::LValue(loc) => writer.to_stack(IR::Mov, loc.to_src(), &self.ty),
            ExprKind::DerefLValue(loc, field) => {
                writer.write(IR::Mov, &MemLocation::r0(), loc.to_src());
                writer.to_stack(IR::Mov, Src::R0Offset(field.offset), &field.ty)
            }
        };
        ResolvedExpr::new(self.ty, loc)
    }
    fn assign(self, writer: &mut Writer, value: Expr) -> Compile<Expr> {
        match self.kind {
            ExprKind::LValue(target) => {
                let src = value.resolve(writer);
                writer.take_from(IR::Mov, &target, src.loc);
                Ok(Expr::resolved(self.ty, target))
            }
            ExprKind::DerefLValue(target, field) => {
                let src = value.resolve(writer);
                writer.write(IR::Mov, &MemLocation::r0(), target.to_src());
                let out = MemLocation::r0_offset(field);
                writer.take_from(IR::Mov, &out, src.loc);
                Ok(Expr::resolved(self.ty, out))
            }
            _ => return Err(Expected("lvalue")),
        }
    }
    fn struct_field(self, field_name: &str) -> Compile<Expr> {
        let field = self.ty.struct_field(field_name)?;
        match self.kind {
            ExprKind::Constant(_) => unreachable!(),
            ExprKind::LValue(m) => Ok(Expr::lvalue(field.ty.clone(), m.struct_field(&field))),
            ExprKind::DerefLValue(m, parent) => Ok(Expr::deref_lvalue(
                field.ty.clone(),
                m,
                StructField {
                    ty: field.ty,
                    offset: parent.offset + field.offset,
                },
            )),
            ExprKind::Resolved(m) => Ok(Expr::lvalue(field.ty.clone(), m.struct_field(&field))),
        }
    }

    fn add_ref(self, writer: &mut Writer) -> Compile<Expr> {
        match self.kind {
            ExprKind::LValue(mem) => {
                let dest_ty = self.ty.add_ref();
                let out = writer.to_stack(IR::LoadAddress, mem.to_src(), &dest_ty);
                Ok(Expr::resolved(dest_ty, out))
            }
            _ => Err(Expected("lvalue")),
        }
    }
    fn deref(self, writer: &mut Writer) -> Compile<Self> {
        match self.kind {
            ExprKind::Resolved(mem) => {
                let dest = MemLocation::r0();
                let ty = self.ty.deref()?;
                writer.take_from(IR::Mov, &dest, mem);
                let loc = writer.to_stack(IR::Mov, Src::R0Offset(0), &ty);
                Ok(Expr::resolved(ty, loc))
            }
            ExprKind::LValue(mem) => Ok(Expr::deref_lvalue(
                self.ty.clone(),
                mem.clone(),
                StructField {
                    ty: self.ty,
                    offset: 0,
                },
            )),
            _ => unimplemented!(),
        }
    }
    fn cast_ty(self, cast_ty: Ty) -> Compile<Self> {
        Ok(Self {
            ty: self.ty.cast(cast_ty)?,
            kind: self.kind,
        })
    }
    // TODO: can evaluation of these be delayed?
    fn index(self, index: Expr, writer: &mut Writer) -> Compile<Self> {
        let item_ty = self.ty.index_ty(&index.ty)?.clone();
        let index_res = index.resolve(writer);
        writer.take_from(IR::Mov, &MemLocation::r0(), index_res.loc);
        let loc = writer.to_stack(IR::Mov, Src::Indexed(0), &item_ty);
        Ok(Expr::resolved(item_ty, loc))
    }
    fn bitset_field(self, field_name: &str, writer: &mut Writer) -> Compile<Self> {
        let field = self.ty.oneof_member(&field_name)?;
        let left_res = self.resolve(writer);
        writer.write(IR::BitTest, &left_res.loc, Src::Immediate(field.index));
        let ty = Ty::bool();
        let loc = writer.to_stack(IR::Mov, Src::R0, &ty);
        Ok(Expr::resolved(ty, loc))
    }
    fn get_constant(&self) -> Option<Word> {
        match &self.kind {
            ExprKind::Constant(value) => Some(*value),
            _ => None,
        }
    }
    // TODO: unary op table
    fn negate(self, writer: &mut Writer) -> Compile<Self> {
        let ty = Ty::int();
        ty.check(&self.ty)?;
        match self.get_constant() {
            Some(value) => Ok(Self::constant(self.ty, -value)),
            None => {
                let tgt = writer.to_stack(IR::Mov, Src::Immediate(0), &ty);
                let operand = self.resolve(writer);
                writer.take_from(IR::Sub, &tgt, operand.loc);
                Ok(Self::resolved(ty, tgt))
            }
        }
    }
}

struct StructBuilder {
    ty: Ty,
    loc: MemLocation,
}

impl StructBuilder {
    fn allocate(ty: Ty, writer: &mut Writer) -> Self {
        let loc = writer.allocate(&ty);
        Self { loc, ty }
    }
    fn field(&self, field_name: &str, value: Expr, writer: &mut Writer) -> Compile<()> {
        let field = self.ty.struct_field(&field_name)?;
        let field_loc = self.loc.struct_field(&field);
        field.ty.check(&value.ty)?;
        let resolved_value = value.resolve(writer);
        writer.take_from(IR::Mov, &field_loc, resolved_value.loc);
        Ok(())
    }
    fn resolve(self) -> Expr {
        Expr::resolved(self.ty, self.loc)
    }
}

struct ArrayBuilder {
    item_ty: Ty,
    loc: MemLocation,
    capacity: Word,
}

impl ArrayBuilder {
    fn init(item_ty: Ty, writer: &mut Writer) -> Self {
        Self {
            item_ty,
            loc: writer.allocate_zero(),
            capacity: 0,
        }
    }
    fn push(&mut self, value: Expr, writer: &mut Writer) -> Compile<()> {
        self.item_ty.check(&value.ty)?;
        self.capacity += 1;
        let value_res = value.resolve(writer);
        self.loc = self.loc.append(&value_res.loc);
        Ok(())
    }
    fn resolve(self) -> Expr {
        let ty = Ty::array(self.item_ty, self.capacity);
        Expr::resolved(ty, self.loc)
    }
}

struct Compiler {
    scope: Scope,
    ty_scope: TyScope,
    writer: Writer,
    lexer: Lexer,
}

impl Compiler {
    fn new(input: &str) -> Self {
        Self {
            lexer: Lexer::new(input),
            writer: Writer::new(),
            scope: Scope::new(),
            ty_scope: TyScope::new(),
        }
    }

    fn bitset_expr(&mut self, ty: Ty) -> Compile<Expr> {
        let mut value = 0;
        loop {
            let key = match self.lexer.type_ident_token()? {
                Some(key) => key,
                _ => break,
            };
            let field = ty.oneof_member(&key)?;
            // TODO: check for dupes
            value |= 1 << field.index;

            if self.lexer.token(Token::Comma)?.is_none() {
                break;
            }
        }
        Ok(Expr::constant(ty, value))
    }

    fn struct_expr(&mut self, ty: Ty) -> Compile<Expr> {
        let builder = StructBuilder::allocate(ty, &mut self.writer);
        loop {
            let field_name = match self.lexer.ident_token()? {
                Some(s) => s,
                None => break,
            };
            self.lexer.expect_token(Token::Colon)?;
            let pair = self.expect_expr()?;
            builder.field(&field_name, pair, &mut self.writer)?;

            if self.lexer.token(Token::Comma)?.is_none() {
                break;
            }
        }
        Ok(builder.resolve())
    }

    fn oneof_member_expr(&mut self, name: &str) -> Compile<Expr> {
        let ty = self.ty_scope.get(name)?;

        self.lexer.expect_token(Token::Dot)?;
        let member_name = self
            .lexer
            .type_ident_token()?
            .ok_or(Expected("oneof member"))?;
        let member = ty.oneof_member(&member_name)?;

        Ok(Expr::constant(ty, member.index))
    }

    fn array_expr(&mut self) -> Compile<Expr> {
        self.lexer.expect_token(Token::SqLeft)?;
        let item_ty = self.expect_type_expr()?;
        self.lexer.expect_token(Token::SqRight)?;
        self.lexer.expect_token(Token::CurlyLeft)?;
        let mut builder = ArrayBuilder::init(item_ty, &mut self.writer);
        loop {
            // TODO: expr must clean up stack when resolving
            match self.expr()? {
                Some(p) => builder.push(p, &mut self.writer)?,
                None => break,
            };
            if self.lexer.token(Token::Comma)?.is_none() {
                break;
            };
        }
        self.lexer.expect_token(Token::CurlyRight)?;
        Ok(builder.resolve())
    }

    fn base_expr(&mut self) -> CompileOpt<Expr> {
        let res = match self.lexer.peek()? {
            Token::ParLeft => {
                self.lexer.advance();
                let expr = self.expect_expr()?;
                self.lexer.expect_token(Token::ParRight)?;
                expr
            }
            Token::TypeIdentifier(name) => {
                self.lexer.advance();
                match self.lexer.peek()? {
                    Token::Dot => self.oneof_member_expr(&name)?,
                    Token::CurlyLeft => {
                        self.lexer.advance();
                        let ty = self.ty_scope.get(&name)?;
                        let out = match self.lexer.peek()? {
                            Token::TypeIdentifier(_) => self.bitset_expr(ty)?,
                            Token::Identifier(_) => self.struct_expr(ty)?,
                            _ => unimplemented!(),
                        };
                        self.lexer.expect_token(Token::CurlyRight)?;
                        out
                    }
                    _ => return Err(Expected("struct or bitset")),
                }
            }
            Token::Array => {
                self.lexer.advance();
                self.array_expr()?
            }
            Token::Integer(int) => {
                self.lexer.advance();
                Expr::constant(Ty::int(), int)
            }
            Token::True => {
                self.lexer.advance();
                Expr::constant(Ty::bool(), 1)
            }
            Token::False => {
                self.lexer.advance();
                Expr::constant(Ty::bool(), 0)
            }
            Token::Identifier(name) => {
                self.lexer.advance();
                let (ty, loc) = self.scope.get(&name)?;
                Expr::lvalue(ty, loc)
            }
            _ => return Ok(None),
        };
        Ok(Some(res))
    }

    fn postfix_expr(&mut self) -> CompileOpt<Expr> {
        let mut left = match self.base_expr()? {
            Some(e) => e,
            None => return Ok(None),
        };
        loop {
            match self.lexer.peek()? {
                Token::SqLeft => {
                    self.lexer.advance();
                    left = match self.expr()? {
                        Some(expr) => left.index(expr, &mut self.writer)?,
                        None => left.deref(&mut self.writer)?,
                    };
                    self.lexer.expect_token(Token::SqRight)?;
                }
                Token::Dot => {
                    self.lexer.advance();
                    left = match self.lexer.peek()? {
                        Token::Identifier(field_name) => {
                            self.lexer.advance();
                            left.struct_field(&field_name)?
                        }
                        Token::TypeIdentifier(field_name) => {
                            self.lexer.advance();
                            left.bitset_field(&field_name, &mut self.writer)?
                        }
                        _ => return Err(Expected("field")),
                    }
                }
                _ => return Ok(Some(left)),
            };
        }
    }

    fn unary_op_expr(&mut self) -> CompileOpt<Expr> {
        let out = match self.lexer.peek()? {
            Token::Minus => {
                self.lexer.advance();
                let operand = self.expect_unary_op_expr()?;
                operand.negate(&mut self.writer)?
            }
            Token::Ampersand => {
                self.lexer.advance();
                let operand = self.expect_unary_op_expr()?;
                operand.add_ref(&mut self.writer)?
            }
            Token::Volatile => {
                self.lexer.advance();
                let operand = self.expect_unary_op_expr()?;
                let res = operand.resolve(&mut self.writer);
                Expr::resolved(res.ty, res.loc)
            }
            _ => return self.postfix_expr(),
        };
        Ok(Some(out))
    }

    fn expect_unary_op_expr(&mut self) -> Compile<Expr> {
        self.unary_op_expr()?.ok_or(Expected("expr"))
    }

    fn op_expr(&mut self) -> CompileOpt<Expr> {
        let left = match self.unary_op_expr()? {
            Some(expr) => expr,
            None => return Ok(None),
        };
        let mut op_parser = OpParser::new(left);

        loop {
            match self.lexer.op()? {
                Some(op) => {
                    op_parser.next(&mut self.writer, op)?;
                    let res = self.expect_unary_op_expr()?;
                    op_parser.push_rhs(res);
                }
                None => return Ok(Some(op_parser.unwind(&mut self.writer)?)),
            }
        }
    }

    fn assign_expr(&mut self) -> CompileOpt<Expr> {
        let left = match self.op_expr()? {
            Some(x) => x,
            None => return Ok(None),
        };
        if self.lexer.token(Token::ColonEq)?.is_none() {
            return Ok(Some(left));
        }
        let expr = self.op_expr()?.ok_or(Expected("expr"))?;
        left.assign(&mut self.writer, expr).map(Some)
    }

    fn as_expr(&mut self) -> CompileOpt<Expr> {
        let value = match self.assign_expr()? {
            Some(x) => x,
            None => return Ok(None),
        };
        if self.lexer.token(Token::As)?.is_none() {
            return Ok(Some(value));
        }
        let ty = self.expect_type_expr()?;
        Ok(Some(value.cast_ty(ty)?))
    }

    fn expr(&mut self) -> CompileOpt<Expr> {
        self.as_expr()
    }

    fn expect_expr(&mut self) -> Compile<Expr> {
        self.expr()?.ok_or(Expected("expr"))
    }

    // ### Bindings, TypeExprs, etc.

    fn binding(&mut self) -> CompileOpt<String> {
        self.lexer.ident_token()
    }

    fn type_binding(&mut self) -> CompileOpt<String> {
        self.lexer.type_ident_token()
    }

    fn struct_ty(&mut self) -> Compile<Ty> {
        let mut fields = TyStruct::new(self.ty_scope.new_type_id());

        self.lexer.expect_token(Token::CurlyLeft)?;
        loop {
            let key = match self.lexer.ident_token()? {
                Some(s) => s,
                None => break,
            };
            self.lexer.expect_token(Token::Colon)?;
            let ty = self.expect_type_expr()?;
            fields.insert(key, ty)?;

            if self.lexer.token(Token::Comma)?.is_none() {
                break;
            }
        }
        self.lexer.expect_token(Token::CurlyRight)?;

        Ok(Ty::struct_(fields))
    }

    fn oneof_ty(&mut self) -> Compile<Ty> {
        let mut data = TyOneOf::new(self.ty_scope.new_type_id());

        self.lexer.expect_token(Token::CurlyLeft)?;
        let mut i = 0;
        loop {
            let key = match self.lexer.type_ident_token()? {
                Some(key) => key,
                _ => break,
            };

            // TODO: allow setting numeric values for oneof members
            // `oneof {Jan := 1, Feb, Mar, Apr}`
            // enforce that numbers are increasing order

            data.insert(key, i)?;
            i += 1;

            if self.lexer.token(Token::Comma)?.is_none() {
                break;
            }
        }
        self.lexer.expect_token(Token::CurlyRight)?;

        Ok(Ty::oneof(data))
    }

    fn type_expr(&mut self) -> CompileOpt<Ty> {
        match self.lexer.peek()? {
            Token::TypeIdentifier(ident) => {
                self.lexer.advance();
                self.ty_scope.get(&ident).map(Some)
            }
            Token::Struct => {
                self.lexer.advance();
                self.struct_ty().map(Some)
            }
            Token::OneOf => {
                self.lexer.advance();
                self.oneof_ty().map(Some)
            }
            _ => Ok(None),
        }
    }

    fn expect_type_expr(&mut self) -> Compile<Ty> {
        self.type_expr()?.ok_or(Expected("type expr"))
    }

    // ### Statements

    fn type_def_stmt(&mut self) -> Compile<()> {
        let tb = self.type_binding()?.ok_or(Expected("type binding"))?;
        self.lexer.expect_token(Token::ColonEq)?;
        let te = self.expect_type_expr()?;
        self.ty_scope.assign(tb, te);
        Ok(())
    }

    fn let_stmt(&mut self) -> Compile<()> {
        let binding = self.binding()?.ok_or(Expected("binding"))?;
        let bind_ty = if self.lexer.token(Token::Colon)?.is_some() {
            let ty = self.expect_type_expr()?;
            Some(ty)
        } else {
            None
        };
        self.lexer.expect_token(Token::ColonEq)?;
        let expr = self.expect_expr()?.resolve(&mut self.writer);
        match bind_ty {
            Some(b) => b.check(&expr.ty)?,
            None => {}
        };
        self.scope.assign(binding, expr.loc, expr.ty);
        Ok(())
    }

    fn stmt(&mut self) -> CompileOpt<()> {
        match self.lexer.peek()? {
            Token::Let => {
                self.lexer.advance();
                self.let_stmt()?;
            }
            Token::Type => {
                self.lexer.advance();
                self.type_def_stmt()?;
            }
            _ => {
                match self.expr()? {
                    Some(expr) => expr.resolve(&mut self.writer),
                    None => return Ok(None),
                };
            }
        };
        Ok(Some(()))
    }

    pub fn program(input: &str) -> Compile<Vec<IR>> {
        let mut parse = Self::new(input);
        loop {
            if parse.stmt()?.is_none() {
                break;
            }
            if parse.lexer.token(Token::Semicolon)?.is_none() {
                break;
            }
        }
        parse.lexer.expect_token(Token::EndOfInput)?;
        Ok(parse.writer.done())
    }
}

fn main() {
    let ir = Compiler::program("").expect("compile");
    Runtime::eval(&ir);
}

#[cfg(test)]
mod test {
    use super::*;
    use super::{EA::*, IR::*};

    fn expect_ir(code: &str, ir: Vec<IR>) {
        assert_eq!(Compiler::program(code), Ok(ir));
    }
    fn expect_result(code: &str, value: Word) {
        let ir = Compiler::program(code).expect("compile");
        let res = Runtime::eval(&ir);
        assert_eq!(res, value);
    }
    fn expect_ir_result(code: &str, ir: Vec<IR>, result: Word) {
        let actual_ir = Compiler::program(code).expect("compile");
        let actual_result: i32 = Runtime::eval(&actual_ir);
        assert_eq!(actual_ir, ir);
        assert_eq!(actual_result, result);
    }
    fn expect_err(code: &str, err: CompileError) {
        assert_eq!(Compiler::program(code), Err(err))
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
        expect_ir_result(
            "volatile 3 * (volatile 4 + volatile 5)",
            vec![
                Mov(PushStack, Immediate(3)),
                Mov(PushStack, Immediate(4)),
                Mov(PushStack, Immediate(5)),
                Add(StackOffset(0), PopStack),
                Mult(StackOffset(0), PopStack),
            ],
            27,
        );
        expect_result("3 * (4 + 5)", 27);
    }

    #[test]
    fn precedence() {
        expect_ir_result(
            "volatile 4 + volatile 5 * volatile 3",
            vec![
                Mov(PushStack, Immediate(4)),
                Mov(PushStack, Immediate(5)),
                Mov(PushStack, Immediate(3)),
                Mult(StackOffset(0), PopStack),
                Add(StackOffset(0), PopStack),
            ],
            19,
        );
        expect_result("4 + 5 * 3", 19);
    }

    #[test]
    fn negation() {
        expect_ir_result("-3", vec![Mov(PushStack, Immediate(-3))], -3);

        expect_ir_result(
            "let a := 3; -a",
            vec![
                Mov(PushStack, Immediate(3)),
                Mov(PushStack, Immediate(0)),
                Mov(PushStack, StackOffset(1)),
                Sub(StackOffset(0), PopStack),
            ],
            -3,
        );

        // TODO: this should clean up
        expect_ir_result(
            "-(volatile 3)",
            vec![
                Mov(PushStack, Immediate(3)),
                Mov(PushStack, Immediate(0)),
                Sub(StackOffset(0), StackOffset(1)),
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
            (volatile ptr)[]
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
        expect_result("(true as Int) +  2", 3);
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
            (volatile p).x
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
            (volatile ptr)[]
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
