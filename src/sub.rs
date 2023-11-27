use crate::block::*;
use crate::expr::*;
use crate::lexer::*;
use crate::memory::*;
use crate::module::*;
use crate::runtime::*;
use crate::stmt::*;
use crate::ty::*;
use crate::{Compile, CompileError::*, CompileOpt};
use std::rc::Rc;

// Calling convention
// [return value, args..., return addr, locals...] top
// caller:
// Sub(SP, sizeof return)
// Mov(Push, arg) ...
// JSR(sub)
// ...
// Add(SP, sizeof args)
// callee:
// ...
// Mov(SP+return, result)
// Add(SP, sizeof locals)
// RTS

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TySub {
    pub params: Vec<Ty>,
    pub ret: Ty,
}

impl TySub {
    pub fn new(params: Vec<Ty>, ret: Ty) -> Self {
        Self { params, ret }
    }
    fn args_size(&self) -> Word {
        self.params.iter().map(|p| p.size()).sum::<Word>()
    }
    pub fn resolve_var(&self, name: &str, ty: &Ty) -> Self {
        Self {
            params: self
                .params
                .iter()
                .map(|p| p.clone().resolve_var(name, ty))
                .collect(),
            ret: self.ret.clone().resolve_var(name, ty),
        }
    }
}

impl TypeExprParser<'_, '_> {
    pub fn sub_ty(&mut self) -> Compile<Ty> {
        self.lexer.expect_token(Token::ParLeft)?;
        let mut params = Vec::new();
        while let Some(param) = self.type_expr()? {
            params.push(param);
            if self.lexer.token(Token::Comma)?.is_none() {
                break;
            }
        }
        self.lexer.expect_token(Token::ParRight)?;
        let ret = if self.lexer.token(Token::Arrow)?.is_some() {
            self.expect_type_expr()?
        } else {
            Ty::void()
        };
        Ok(Ty::sub(TySub::new(params, ret)))
    }
}

impl ExprParser<'_, '_, '_, '_, '_, '_> {
    pub fn call_args(&mut self, expr: Expr) -> Compile<Expr> {
        let mut builder = self.compiler.begin_call(expr)?;
        while let Some(expr) = self.expr()? {
            self.compiler.call_arg(&mut builder, expr)?;
            if self.lexer.token(Token::Comma)?.is_none() {
                break;
            }
        }
        self.compiler.end_call(builder)
    }
}

struct CallBuilder {
    sub_index: SubIndex,
    ty_sub: Rc<TySub>,
    return_block: Block,
    current_arg: usize,
}

impl ExprCompiler<'_, '_, '_> {
    fn begin_call(&mut self, callee: Expr) -> Compile<CallBuilder> {
        let sub_index = callee.sub_index()?;
        let ty_sub = callee.ty.get_sub()?;
        let return_block = self.memory.allocate(ty_sub.ret.size());
        Ok(CallBuilder {
            sub_index,
            ty_sub,
            return_block,
            current_arg: 0,
        })
    }
    fn call_arg(&mut self, builder: &mut CallBuilder, arg: Expr) -> Compile<()> {
        let params = &builder.ty_sub.params;
        if builder.current_arg >= params.len() {
            return Err(Expected("fewer params"));
        }
        params[builder.current_arg].check(&arg.ty)?;
        arg.resolve(self.memory, ExprTarget::Stack);
        builder.current_arg += 1;
        Ok(())
    }
    fn end_call(&mut self, builder: CallBuilder) -> Compile<Expr> {
        if builder.current_arg != builder.ty_sub.params.len() {
            return Err(Expected("more params"));
        }
        self.memory
            .call_sub(builder.sub_index, builder.ty_sub.args_size());
        Ok(Expr::resolved(
            builder.ty_sub.ret.clone(),
            builder.return_block,
        ))
    }
}

impl StmtParser<'_, '_, '_, '_, '_> {
    pub fn return_stmt(&mut self) -> Compile<()> {
        let maybe_expr = self.expr()?;
        self.compiler.return_sub(maybe_expr)?;
        Ok(())
    }
}

impl StmtCompiler<'_, '_> {
    fn return_sub(&mut self, expr: Option<Expr>) -> Compile<()> {
        match expr {
            Some(expr) => {
                self.check_return(&expr.ty)?;
                expr.resolve(
                    self.memory,
                    ExprTarget::Reference(Reference::block(self.return_expr.block)),
                );
            }
            None => {
                self.check_return(&Ty::void())?;
            }
        };
        self.memory.return_sub();
        Ok(())
    }
    fn end_sub(&mut self) -> Compile<()> {
        if !self.did_return {
            if self.return_expr.ty == Ty::void() {
                self.memory.return_sub();
            } else {
                return Err(Expected("return"));
            }
        }

        Ok(())
    }
    fn check_return(&mut self, ty: &Ty) -> Compile<()> {
        self.did_return = true;
        self.return_expr.ty.check(ty)
    }
}

impl ModuleParser {
    fn param_binding(&mut self) -> CompileOpt<String> {
        self.lexer.ident_token()
    }

    fn sub_params(&mut self, builder: &mut SubBuilder) -> Compile<()> {
        self.lexer.expect_token(Token::ParLeft)?;
        while let Some(binding) = self.param_binding()? {
            self.lexer.expect_token(Token::Colon)?;
            let ty = self.expect_type_expr()?;
            builder.add_param(binding, ty);

            if self.lexer.token(Token::Comma)?.is_none() {
                break;
            }
        }
        self.lexer.expect_token(Token::ParRight)?;

        if self.lexer.token(Token::Arrow)?.is_some() {
            let ty = self.expect_type_expr()?;
            builder.returns(ty);
        };

        Ok(())
    }

    pub fn sub(&mut self) -> Compile<()> {
        let name = self.lexer.ident_token()?.ok_or(Expected("name"))?;
        let mut builder = SubBuilder::new(name);
        self.sub_params(&mut builder)?;
        self.lexer.expect_token(Token::Do)?;

        let mut stmt_compiler = self.compiler.begin_sub(builder);
        let mut sp = StmtParser::new(&mut self.lexer, &mut stmt_compiler, &mut self.ty_scope);
        sp.block()?;
        sp.compiler.end_sub()?;
        self.lexer.expect_token(Token::End)?;
        Ok(())
    }
}

struct SubBuilder {
    name: String,
    params: Vec<(String, Ty)>,
    return_type: Ty,
}

impl SubBuilder {
    fn new(name: String) -> Self {
        Self {
            name,
            params: Vec::new(),
            return_type: Ty::void(),
        }
    }
    fn add_param(&mut self, name: String, ty: Ty) {
        self.params.push((name, ty));
    }
    pub fn returns(&mut self, ty: Ty) {
        self.return_type = ty;
    }
}

impl ModuleCompiler {
    fn begin_sub(&mut self, builder: SubBuilder) -> StmtCompiler {
        let params = builder.params.iter().map(|(_, ty)| ty.clone()).collect();
        let ty_sub = TySub::new(params, builder.return_type.clone());
        // frame offset is negative for args & return slot
        let mut frame_offset = -(ty_sub.args_size() + 1);
        let return_expr = ResolvedExpr {
            block: Block::new(frame_offset, builder.return_type.size()),
            ty: builder.return_type,
        };

        let ty = Ty::sub(ty_sub);
        let sub_index = self.memory.sub();
        self.module_scope
            .insert(builder.name, ModuleRecord { ty, sub_index });

        let mut stmt_compiler =
            StmtCompiler::from_module(&mut self.memory, &mut self.module_scope, return_expr);

        for (key, ty) in builder.params {
            frame_offset += ty.size();
            stmt_compiler.insert_scope_record(key, ScopeRecord { frame_offset, ty });
        }

        debug_assert!(frame_offset == -1);
        stmt_compiler
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SubIndex {
    index: Word,
}

impl Memory {
    fn sub(&self) -> SubIndex {
        SubIndex {
            index: self.output.len() as Word,
        }
    }
    pub fn done_program(self, main_sub: SubIndex) -> CompileResult {
        CompileResult {
            code: self.output,
            entry_point: main_sub.index,
        }
    }
    fn call_sub(&mut self, sub_index: SubIndex, args_size: Word) {
        self.output.push(IR::Call(sub_index.index));
        self.drop(args_size);
    }
    fn return_sub(&mut self) {
        self.drop(self.current_frame_offset);
        self.output.push(IR::Return);
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::expr::test::ExprFixture;
    use crate::runtime::{Register::*, EA::*, IR::*};

    #[test]
    fn subroutine_calls() {
        let mut fx = ExprFixture::new();
        fx.module_scope.insert(
            "s".to_string(),
            ModuleRecord {
                ty: Ty::sub(TySub::new(vec![Ty::int(), Ty::bool()], Ty::int())),
                sub_index: SubIndex { index: 100 },
            },
        );
        fx.scope.insert(
            "val".to_string(),
            ScopeRecord {
                frame_offset: 0,
                ty: Ty::int(),
            },
        );

        fx.expect_err("val(1, true)", Expected("subroutine"));
        fx.expect_err("s(1)", Expected("more params"));
        fx.expect_err("s(1, false, 2)", Expected("fewer params"));
        fx.expect_err("s(1, 2)", ExpectedType(Ty::bool(), Ty::int()));

        fx.expect_ir(
            "s(3, true)",
            vec![
                // allocate space for return
                Sub(Register(SP), Immediate(1)),
                // 3
                Mov(PreDec(SP), Immediate(3)),
                // true
                Mov(PreDec(SP), Immediate(1)),
                // call s
                Call(100),
                // clean up args
                Add(Register(SP), Immediate(2)),
            ],
        );
    }
}
