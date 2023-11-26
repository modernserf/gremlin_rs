use crate::block::*;
use crate::expr::*;
use crate::lexer::*;
use crate::memory::*;
use crate::runtime::*;
use crate::stmt::*;
use crate::ty::*;
use crate::{Compile, CompileError::*, CompileOpt};
use std::collections::HashMap;

pub struct ModuleParser {
    compiler: ModuleCompiler,
    lexer: Lexer,
    ty_scope: TyScope,
}

impl ModuleParser {
    pub fn program(input: &str) -> Compile<CompileResult> {
        let mut parse = Self::new(Lexer::new(input), ModuleCompiler::init(), TyScope::new());
        parse.module()?;
        parse.lexer.expect_token(Token::EndOfInput)?;
        parse.compiler.resolve()
    }
    #[allow(dead_code)]
    pub fn script(input: &str) -> Compile<Vec<IR>> {
        let mut p = Self::new(Lexer::new(input), ModuleCompiler::init(), TyScope::new());
        let mut stmt_compiler = p.compiler.script_scope();
        let mut sp = StmtParser::new(&mut p.lexer, &mut stmt_compiler, &mut p.ty_scope);
        sp.block()?;
        p.lexer.expect_token(Token::EndOfInput)?;
        let res = p.compiler.done();
        Ok(res)
    }
    fn new(lexer: Lexer, compiler: ModuleCompiler, ty_scope: TyScope) -> Self {
        Self {
            lexer,
            compiler,
            ty_scope,
        }
    }

    fn expect_type_expr(&mut self) -> Compile<Ty> {
        let mut p = TypeExprParser::new(&mut self.lexer, &mut self.ty_scope);
        let ty = p.expect_type_expr()?;
        Ok(ty)
    }

    fn type_binding(&mut self) -> CompileOpt<String> {
        self.lexer.type_ident_token()
    }

    // TODO: remove statement-level typedefs?
    fn type_def_stmt(&mut self) -> Compile<()> {
        let tb = self.type_binding()?.ok_or(Expected("type binding"))?;
        self.lexer.expect_token(Token::ColonEq)?;
        let te = self.expect_type_expr()?;
        self.ty_scope.assign(tb, te);
        self.lexer.expect_token(Token::Semicolon)?;
        Ok(())
    }

    fn binding(&mut self) -> CompileOpt<String> {
        self.lexer.ident_token()
    }

    fn sub_params(&mut self, builder: &mut SubBuilder) -> Compile<()> {
        self.lexer.expect_token(Token::ParLeft)?;
        while let Some(binding) = self.binding()? {
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

    fn sub(&mut self) -> Compile<()> {
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

    fn module_stmt(&mut self) -> CompileOpt<()> {
        match self.lexer.peek()? {
            Token::Type => {
                self.lexer.advance();
                self.type_def_stmt()?;
                Ok(Some(()))
            }
            Token::Sub => {
                self.lexer.advance();
                self.sub()?;
                Ok(Some(()))
            }
            _ => Ok(None),
        }
    }

    fn module(&mut self) -> Compile<()> {
        loop {
            if self.module_stmt()?.is_none() {
                break;
            }
        }
        self.lexer.expect_token(Token::EndOfInput)
    }
}

pub struct ModuleCompiler {
    memory: Memory,
    module_scope: HashMap<String, ModuleRecord>,
}

pub struct ModuleRecord {
    pub ty: Ty,
    pub sub_index: SubIndex,
}

impl ModuleCompiler {
    pub fn init() -> Self {
        Self {
            module_scope: HashMap::new(),
            memory: Memory::new(),
        }
    }
    pub fn resolve(self) -> Compile<CompileResult> {
        let entry_point = self
            .module_scope
            .get("main")
            .ok_or(Expected("main"))?
            .sub_index;
        Ok(self.memory.done_program(entry_point))
    }
    pub fn script_scope(&mut self) -> StmtCompiler {
        StmtCompiler::from_module(
            &mut self.memory,
            &mut self.module_scope,
            ResolvedExpr::void(),
        )
    }
}

pub struct SubBuilder {
    name: String,
    params: Vec<(String, Ty)>,
    return_type: Ty,
}

impl SubBuilder {
    pub fn new(name: String) -> Self {
        Self {
            name,
            params: Vec::new(),
            return_type: Ty::void(),
        }
    }
    pub fn add_param(&mut self, name: String, ty: Ty) {
        self.params.push((name, ty));
    }
    pub fn returns(&mut self, ty: Ty) {
        self.return_type = ty;
    }
}

impl ModuleCompiler {
    pub fn begin_sub(&mut self, builder: SubBuilder) -> StmtCompiler {
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
    pub fn done(self) -> Vec<IR> {
        self.memory.done()
    }
}
