use crate::expr::*;
use crate::lexer::*;
use crate::memory::*;
use crate::runtime::*;
use crate::stmt::*;
use crate::sub::*;
use crate::ty::*;
use crate::{Compile, CompileError::*, CompileOpt};
use std::collections::HashMap;

pub struct ModuleParser {
    pub compiler: ModuleCompiler,
    pub lexer: Lexer,
    pub ty_scope: TyScope,
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

    pub fn expect_type_expr(&mut self) -> Compile<Ty> {
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
    pub memory: Memory,
    pub module_scope: HashMap<String, ModuleRecord>,
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
    pub fn done(self) -> Vec<IR> {
        self.memory.done()
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
