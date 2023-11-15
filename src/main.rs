mod ast;
mod compiler;
mod ir;
mod lexer;
mod parser;
mod runtime;
mod source_info;
mod token;
mod typechecker;
mod typed_ast;

use crate::compiler::Compiler;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::runtime::Runtime;
use crate::typechecker::TypeChecker;

fn eval(program: &str) -> u32 {
    let tok = Lexer::lex(&program);
    let ast = Parser::parse_body(tok).expect("ast");
    let tc = TypeChecker::check(&ast).expect("typecheck");
    let ir = Compiler::compile(&tc).expect("ir");
    Runtime::eval(&ir)
}

fn main() {
    eval("123");
}

#[cfg(test)]
mod test {
    use super::*;

    fn assert_expr_eq(str: &str, expected: u32) {
        assert_eq!(eval(str), expected);
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
                let foo := 6
                let ptr := &foo
                @ptr
            ",
            6,
        );
    }

    #[test]
    fn assignment() {
        assert_expr_eq(
            "
            let foo := 1
            foo := 3
            let bar := 5
            foo
        ",
            3,
        );
    }

    #[test]
    fn bools() {
        assert_expr_eq("false", 0);
        assert_expr_eq("true", 1);

        assert_expr_eq("true and false", 0);
        assert_expr_eq("true or false", 1);
        assert_expr_eq("not false", 1);
    }

    #[test]
    fn type_casting() {
        assert_expr_eq(
            "
            let a := true
            let b := (a as int) + 1
            b
        ",
            2,
        )
    }

    #[test]
    fn pointer_math_casting() {
        assert_expr_eq(
            "
                # stack ptr goes high to low
                let foo_1 := 20
                let foo_0 := 10
                @((&foo_0 as int) + 1 as &int)
            ",
            20,
        );
    }

    #[test]
    fn type_alias() {
        assert_expr_eq(
            "
                type word := int
                let x: word := 123
                x
            ",
            123,
        )
    }

    #[test]
    fn longs() {
        assert_expr_eq("123l", 123);
        assert_expr_eq(
            "
            let a := 123l;
            let b := &a;
            @b
        ",
            123,
        );
        assert_expr_eq(
            "
            let a := 123l;
            a := 456l;
            a
        ",
            456,
        );
    }

    #[test]
    fn structs() {
        assert_expr_eq(
            "
            type Pair := struct
                x: int
                y: int
            end

            Pair { x := 1; y := 2 }.x;
        ",
            1,
        );
        assert_expr_eq(
            "
            type Pair := struct
                x: int
                y: int
            end
            let p := Pair { x := 1; y := 2 };
            p.x
        ",
            1,
        );
        assert_expr_eq(
            "
            type Pair := struct
                x: int
                y: int
            end
            type Rect := struct
                top_left: Pair
                bottom_right: Pair
            end

            let r := Rect {
                top_left := Pair { x := 1; y := 2 }
                bottom_right := Pair { x := 3; y := 4 }
            }
            let bottom_right := r.bottom_right;

            bottom_right.x
        ",
            3,
        );
        assert_expr_eq(
            "
            type Pair := struct
                x: int
                y: int
            end

            let p := Pair { x := 1; y := 2 }
            p.x := 3
            p.x
        ",
            3,
        );
        assert_expr_eq(
            "
            type Pair := struct
                x: int
                y: int
            end
            
            let p := Pair { x := 1; y := 2 }
            let ptr := &p.x
            p.x := 5
            @ptr
        ",
            5,
        )
    }

    #[test]
    fn oneof() {
        assert_expr_eq(
            "
            type TrafficLight := oneof Green Yellow Red end
            TrafficLight.Yellow
        ",
            1,
        );
        assert_expr_eq(
            "
                type TrafficLight := oneof Green Yellow Red end
                TrafficLight{Green;Red}
            ",
            0b101,
        );
        assert_expr_eq(
            "
                type TrafficLight := oneof Green Yellow Red end
                let lights := TrafficLight{Yellow}
                lights.Yellow # true
            ",
            1,
        );
        // TODO: lights.Yellow := true
        // TODO: set (bitwise) operations
        // TODO: what is type syntax for oneof set?
    }
}
