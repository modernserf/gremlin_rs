use crate::ast::*;
use crate::source_info::SourceInfo;
use crate::token::{Identifier, Tok, TokKind};

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
    pub fn parse_body(tokens: Vec<Tok>) -> Parse<Vec<Stmt>> {
        let mut parser = Self::new(tokens);
        let body = parser.body()?;
        parser.expect_token(TokKind::EndOfInput)?;
        Ok(body)
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

    fn expect_token(&mut self, tok: TokKind) -> Parse<SourceInfo> {
        if self.peek() == tok {
            let source_info = self.peek_source();
            self.advance();
            Ok(source_info)
        } else {
            Err(ParseError::expected_token(tok, self.peek_source()))
        }
    }

    fn expect_identifier(&mut self) -> Parse<Identifier> {
        match self.peek() {
            TokKind::Identifier(payload) => {
                self.advance();
                Ok(payload)
            }
            _ => Err(ParseError::expected("identifier", self.peek_source())),
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

    fn body(&mut self) -> Parse<Vec<Stmt>> {
        let mut out = Vec::new();
        loop {
            match self.stmt()? {
                Some(stmt) => {
                    out.push(stmt);
                }
                None => break,
            }
        }
        Ok(out)
    }

    fn stmt(&mut self) -> ParseOpt<Stmt> {
        let start_source = self.peek_source();
        match self.peek() {
            TokKind::Let => {
                self.advance();
                let binding = self.expect_p("binding", Self::binding)?;

                let ty = match self.peek() {
                    TokKind::Colon => {
                        self.advance();
                        Some(self.expect_p("type expr", Self::type_expr)?)
                    }
                    _ => None,
                };

                self.expect_token(TokKind::ColonEq)?;
                let expr = self.expect_p("expr", Self::expr)?;
                let source_info = start_source.span(expr.source_info);
                Ok(Some(Stmt {
                    kind: StmtKind::Let(Box::new(LetStmt { binding, expr, ty })),
                    source_info,
                }))
            }
            TokKind::Type => {
                self.advance();
                let identifier = self.expect_identifier()?;
                self.expect_token(TokKind::ColonEq)?;
                let ty = self.expect_p("type expr", Self::type_expr)?;
                let source_info = start_source.span(ty.source_info);
                Ok(Some(Stmt {
                    kind: StmtKind::TypeDef(Box::new(TypeDef {
                        identifier: identifier.value,
                        ty,
                    })),
                    source_info,
                }))
            }
            TokKind::Semicolon => {
                self.advance();
                Ok(Some(Stmt {
                    kind: StmtKind::Noop,
                    source_info: start_source,
                }))
            }
            _ => {
                if let Some(expr) = self.expr()? {
                    if self.peek() == TokKind::ColonEq {
                        self.advance();
                        let value = self.expect_p("expr", Self::expr)?;
                        let source_info = start_source.span(value.source_info);
                        Ok(Some(Stmt {
                            kind: StmtKind::Assign(Box::new(AssignStmt {
                                target: expr,
                                expr: value,
                            })),
                            source_info,
                        }))
                    } else {
                        let source_info = expr.source_info;
                        Ok(Some(Stmt {
                            kind: StmtKind::Expr(Box::new(expr)),
                            source_info,
                        }))
                    }
                } else {
                    Ok(None)
                }
            }
        }
    }

    fn expr(&mut self) -> ParseOpt<Expr> {
        self.as_expr()
    }

    fn as_expr(&mut self) -> ParseOpt<Expr> {
        if let Some(expr) = self.or_expr()? {
            let start_source = expr.source_info;
            match self.peek() {
                TokKind::As => {
                    self.advance();
                    let ty = self.expect_p("type expr", Self::type_expr)?;
                    let source_info = start_source.span(ty.source_info);
                    Ok(Some(Expr {
                        kind: ExprKind::As(Box::new(AsExpr { expr, ty })),
                        source_info,
                    }))
                }
                _ => Ok(Some(expr)),
            }
        } else {
            Ok(None)
        }
    }

    fn or_expr(&mut self) -> ParseOpt<Expr> {
        self.left_op_expr(Self::and_expr, |p| match p.peek() {
            TokKind::Or => {
                p.advance();
                Ok(Some(BinOpKind::Or))
            }
            _ => Ok(None),
        })
    }

    fn and_expr(&mut self) -> ParseOpt<Expr> {
        self.left_op_expr(Self::add_expr, |p| match p.peek() {
            TokKind::And => {
                p.advance();
                Ok(Some(BinOpKind::And))
            }
            _ => Ok(None),
        })
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
            TokKind::Ampersand => {
                self.advance();
                UnOpKind::Ref
            }
            TokKind::At => {
                self.advance();
                UnOpKind::Deref
            }
            TokKind::Not => {
                self.advance();
                UnOpKind::Not
            }
            _ => return self.postfix_expr(),
        };
        let expr = self.expect_p("expr", Self::un_op_expr)?;
        let source_info = op_source.span(expr.source_info);
        Ok(Some(Expr {
            kind: ExprKind::UnaryOp(Box::new(UnaryOp { operator, expr })),
            source_info,
        }))
    }

    fn postfix_expr(&mut self) -> ParseOpt<Expr> {
        let mut expr = if let Some(expr) = self.base_expr()? {
            expr
        } else {
            return Ok(None);
        };
        loop {
            match self.peek() {
                TokKind::Dot => {
                    self.advance();
                    let end_source = self.peek_source();
                    let field = self.expect_identifier()?;
                    let source_info = expr.source_info.span(end_source);
                    expr = Expr {
                        kind: ExprKind::Field(Box::new(FieldExpr {
                            expr,
                            field: field.value,
                        })),
                        source_info,
                    }
                }
                _ => {
                    return Ok(Some(expr));
                }
            }
        }
    }

    fn base_expr(&mut self) -> ParseOpt<Expr> {
        let start_source = self.peek_source();
        match self.peek() {
            TokKind::ParLeft => {
                self.advance();
                let expr = self.expect_p("expr", Self::expr)?;
                self.expect_token(TokKind::ParRight)?;
                Ok(Some(expr))
            }
            TokKind::IntLiteral(payload) => {
                self.advance();
                Ok(Some(Expr {
                    kind: ExprKind::Int(payload),
                    source_info: start_source,
                }))
            }
            TokKind::LongLiteral(payload) => {
                self.advance();
                Ok(Some(Expr {
                    kind: ExprKind::Long(payload),
                    source_info: start_source,
                }))
            }
            TokKind::Identifier(payload) => {
                self.advance();

                if TokKind::CurlyLeft == self.peek() {
                    self.advance();
                    let body = self.body()?;
                    let end_source = self.expect_token(TokKind::CurlyRight)?;
                    Ok(Some(Expr {
                        kind: ExprKind::Struct(Box::new(StructExpr {
                            name: payload.value,
                            body,
                        })),
                        source_info: start_source.span(end_source),
                    }))
                } else {
                    Ok(Some(Expr {
                        kind: ExprKind::Ident(IdentExpr {
                            value: payload.value,
                        }),
                        source_info: start_source,
                    }))
                }
            }
            TokKind::True => {
                self.advance();
                Ok(Some(Expr {
                    kind: ExprKind::True,
                    source_info: start_source,
                }))
            }
            TokKind::False => {
                self.advance();
                Ok(Some(Expr {
                    kind: ExprKind::False,
                    source_info: start_source,
                }))
            }
            _ => Ok(None),
        }
    }

    fn binding(&mut self) -> ParseOpt<Bind> {
        let start_source = self.peek_source();
        match self.peek() {
            TokKind::Identifier(payload) => {
                self.advance();
                Ok(Some(Bind {
                    kind: BindKind::Ident(IdentBind {
                        value: payload.value,
                    }),
                    source_info: start_source,
                }))
            }
            _ => Ok(None),
        }
    }

    fn type_expr(&mut self) -> ParseOpt<TyExpr> {
        let start_source = self.peek_source();
        match self.peek() {
            TokKind::Identifier(payload) => {
                self.advance();
                Ok(Some(TyExpr {
                    kind: TyExprKind::Identifier(IdentTyExpr {
                        value: payload.value,
                    }),
                    source_info: start_source,
                    ref_level: 0,
                }))
            }
            TokKind::Ampersand => {
                self.advance();
                let mut next = self.expect_p("type expr", Self::type_expr)?;
                next.ref_level += 1;
                next.source_info = start_source.span(next.source_info);
                Ok(Some(next))
            }
            TokKind::Struct => {
                self.advance();
                let mut fields = Vec::new();
                let end_source;
                loop {
                    if let Some(es) = self.match_token(TokKind::End)? {
                        end_source = es;
                        break;
                    }
                    let key = self.expect_identifier()?;
                    self.expect_token(TokKind::Colon)?;
                    let ty = self.expect_p("type expr", Self::type_expr)?;
                    self.match_token(TokKind::Semicolon)?;
                    fields.push(StructTyField { key: key.value, ty });
                }
                Ok(Some(TyExpr {
                    kind: TyExprKind::Struct(StructTyExpr { fields }),
                    source_info: start_source.span(end_source),
                    ref_level: 0,
                }))
            }
            _ => Ok(None),
        }
    }
    fn match_token(&mut self, tok: TokKind) -> ParseOpt<SourceInfo> {
        if tok == self.peek() {
            let out = self.peek_source();
            self.advance();
            Ok(Some(out))
        } else {
            Ok(None)
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lexer::Lexer;
    use crate::token::IntLiteral;

    fn assert_expr_eq(str: &str, expected: Expr) {
        let result = Parser::parse_body(Lexer::lex(str)).expect("expr");
        let source_info = expected.source_info;
        assert_eq!(
            result,
            vec![Stmt {
                kind: StmtKind::Expr(Box::new(expected),),
                source_info
            }]
        );
    }

    fn assert_err(str: &str, expected: ParseError) {
        let result = Parser::parse_body(Lexer::lex(str)).expect_err("parse error");
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
                        kind: ExprKind::Int(IntLiteral { value: 1 }),
                        source_info: SourceInfo {
                            start: 2,
                            length: 1,
                        },
                    },
                    right: Expr {
                        kind: ExprKind::Int(IntLiteral { value: 2 }),
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
