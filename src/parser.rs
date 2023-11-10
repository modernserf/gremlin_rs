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

    fn repeat<T>(&mut self, parse_item: fn(&mut Self) -> ParseOpt<T>) -> Parse<Vec<T>> {
        let mut output: Vec<T> = vec![];
        loop {
            if let Some(item) = parse_item(self)? {
                output.push(item);
            } else {
                return Ok(output);
            }
        }
    }

    fn body(&mut self) -> Parse<Vec<Stmt>> {
        self.repeat(Self::stmt)
    }

    fn stmt(&mut self) -> ParseOpt<Stmt> {
        let start_source = self.peek_source();
        match self.peek() {
            TokKind::Let => {
                self.advance();
                let binding = self.expect_p("binding", Self::binding)?;
                // TODO: type assertion
                self.expect_token(TokKind::ColonEq)?;
                let expr = self.expect_p("expr", Self::expr)?;
                let source_info = start_source.span(expr.source_info);
                Ok(Some(Stmt {
                    kind: StmtKind::Let(Box::new(LetStmt { binding, expr })),
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
        self.or_expr()
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
                    kind: ExprKind::Int(payload),
                    source_info,
                }))
            }
            TokKind::Identifier(payload) => {
                let source_info = self.peek_source();
                self.advance();
                Ok(Some(Expr {
                    kind: ExprKind::Ident(IdentExpr {
                        value: payload.value,
                    }),
                    source_info,
                }))
            }
            TokKind::True => {
                let source_info = self.peek_source();
                self.advance();
                Ok(Some(Expr {
                    kind: ExprKind::True,
                    source_info,
                }))
            }
            TokKind::False => {
                let source_info = self.peek_source();
                self.advance();
                Ok(Some(Expr {
                    kind: ExprKind::False,
                    source_info,
                }))
            }
            _ => Ok(None),
        }
    }

    fn binding(&mut self) -> ParseOpt<Bind> {
        match self.peek() {
            TokKind::Identifier(payload) => {
                let source_info = self.peek_source();
                self.advance();
                Ok(Some(Bind {
                    kind: BindKind::Ident(IdentBind {
                        value: payload.value,
                    }),
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
