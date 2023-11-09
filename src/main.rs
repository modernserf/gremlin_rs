/* Gremlin grammar
Stmt =
    | "let" Binding (":" TyExpr)? ":=" Expr
    | Expr ":=" Expr
    | "while" Expr "do" Block "end"
    | "return" Expr?
    | "func" identifier "(" (Binding ":" TyExpr) ** "," ")" ("->" TyExpr)? "do" Block "end"
    | "struct" identifier "do" (identifier ":" TyExpr) ** "," "end"
    | Expr
Block = Stmt ** ";"
Binding = identifier
TyExpr =
    | identifier "<" TyExpr ** "," ">"
    | identifier
    | "&" TyExpr
Expr =
    | "(" Expr ")"
    | "if" Expr "then" Block ("else" "if" Expr "then" Block)* ("else" Block)? "end"
    | Expr "(" Expr ** "," ")"
    | Expr "." identifier
    | Expr Op Expr
    | Op Expr
    | Expr "as" TyExpr
    | identifier
    | number
    | string
    | "true"
    | "false"
Op = "+" | "-" | "*" | "/" | "&" | "@"
Comment = "#" ... eol
*/

mod ast;
mod lexer;
mod parser;
mod runtime;
mod source_info;
mod token;

use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::runtime::Interpreter;

fn main() {
    println!("Hello, world!");
    Interpreter::new().eval_body(&Parser::parse_body(Lexer::lex("123")).unwrap());
}

#[cfg(test)]
mod test {
    use super::*;

    fn assert_expr_eq(str: &str, expected: u128) {
        let mut interpreter = Interpreter::new();
        interpreter.eval_body(&Parser::parse_body(Lexer::lex(str)).expect("expr"));

        assert_eq!(interpreter.pop_stk(), expected);
    }

    #[test]
    fn integers() {
        assert_expr_eq("123", 123);
    }

    #[test]
    fn ignore_whitespace() {
        assert_expr_eq("       123     ", 123);
    }

    #[test]
    fn ignore_comments() {
        assert_expr_eq(
            " 
        
        # before

        123 # a comment
        
        ",
            123,
        );
    }

    #[test]
    fn addition() {
        assert_expr_eq("1 + 2 + 3", 6);
    }

    #[test]
    fn add_mult() {
        assert_expr_eq("2 * 4 + 5", 13);
    }

    #[test]
    fn add_mult_paren() {
        assert_expr_eq("2 * (4 + 5)", 18);
    }

    #[test]
    fn add_mult_pemdas() {
        assert_expr_eq("5 + 4 * 2", 13);
    }

    #[test]
    fn identifiers() {
        assert_expr_eq(
            "
            let a := 123
            let b := 456
            let c := 789
            b
            ",
            456,
        )
    }

    #[test]
    fn ref_deref() {
        assert_expr_eq(
            "
                let foo := 123
                let ptr := &foo
                @ptr
            ",
            123,
        );
        assert_expr_eq(
            "
                # stack ptr goes high to low
                let foo_1 := 456
                let foo_0 := 123
                @(&foo_0 + 1) 
            ",
            456,
        );
    }
}
