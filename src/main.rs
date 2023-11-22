mod compiler;
mod expr;
mod lexer;
mod memory;
mod op;
mod parser;
mod record;
mod runtime;
mod subroutine;
mod ty;

use std::collections::HashMap;

use crate::lexer::*;
use crate::op::*;
use crate::parser::*;
use crate::runtime::*;
use crate::ty::*;

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
    InvalidRef,
}
use CompileError::*;

pub type Compile<T> = Result<T, CompileError>;
pub type CompileOpt<T> = Result<Option<T>, CompileError>;

fn main() {
    let result = Parser::program("").expect("compile");
    Runtime::eval_result(result);
}

#[cfg(test)]
mod test {
    use super::*;
    use super::{EA::*, IR::*};

    fn expect_ir(code: &str, ir: Vec<IR>) {
        assert_eq!(Parser::script(code), Ok(ir));
    }
    fn expect_result(code: &str, _ir: Vec<IR>, value: Word) {
        let ir = Parser::script(code).expect("compile");
        let res = Runtime::eval(&ir);
        assert_eq!(res, value);
    }
    fn expect_ir_result(code: &str, ir: Vec<IR>, result: Word) {
        let actual_ir = Parser::script(code).expect("compile");
        assert_eq!(actual_ir, ir);
        let actual_result: i32 = Runtime::eval(&actual_ir);
        assert_eq!(actual_result, result);
    }
    fn expect_err(code: &str, err: CompileError) {
        assert_eq!(Parser::script(code), Err(err))
    }
    #[allow(dead_code)]
    fn run_ir(ir: Vec<IR>) {
        let res = Runtime::eval(&ir);
        dbg!(res);
    }

    fn expect_program(code: &str, ir: Vec<IR>) {
        let result = Parser::program(code).expect("compile");
        Runtime::eval_result(result.clone());
        assert_eq!(result.code, ir);
    }

    #[test]
    fn empty_program() {
        expect_ir("", vec![]);
    }

    #[test]
    fn integers() {
        expect_ir_result("123;", vec![Mov(PushStack, Immediate(123))], 123);
    }

    #[test]
    fn whitespace() {
        expect_ir(
            "
            123;

            ",
            vec![Mov(PushStack, Immediate(123))],
        )
    }

    #[test]
    fn comments() {
        expect_ir(
            "123; # This is a comment",
            vec![Mov(PushStack, Immediate(123))],
        )
    }

    #[test]
    fn unexpected_char() {
        expect_err(" £ ", UnexpectedChar)
    }

    #[test]
    fn bools() {
        expect_ir("true;", vec![Mov(PushStack, Immediate(1))]);
        expect_ir("false;", vec![Mov(PushStack, Immediate(0))]);
    }

    #[test]
    fn let_stmts() {
        expect_ir_result(
            "
                let x := 1;
                let y := 2;
                x;
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
            x;
        ",
            vec![
                Mov(PushStack, Immediate(1)),
                Mov(PushStack, Immediate(3)),
                Mov(StackOffset(1), StackOffset(0)),
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
            x + 3 + y;
        ",
            vec![
                Mov(PushStack, Immediate(1)),        // [x: 1]
                Mov(PushStack, Immediate(2)),        // [y: 2, x: 1]
                Mov(PushStack, StackOffset(1)),      // [1, y: 2, x: 1]
                Add(StackOffset(0), Immediate(3)),   // [4, y: 2, x: 1]
                Add(StackOffset(0), StackOffset(1)), // [6, y: 2, x: 1]
            ],
            6,
        );
    }

    #[test]
    fn typechecked_arithmetic() {
        expect_err("1 + true;", ExpectedType(Ty::int(), Ty::bool()));
        expect_err("true + 1;", ExpectedType(Ty::int(), Ty::bool()));
    }

    #[test]
    fn parens() {
        expect_ir_result(
            "volatile 3 * (volatile 4 + volatile 5);",
            vec![
                Mov(PushStack, Immediate(3)),
                Mov(PushStack, Immediate(4)),
                Mov(PushStack, Immediate(5)),
                Add(StackOffset(1), StackOffset(0)),
                Mult(StackOffset(2), StackOffset(1)),
                Add(SP, Immediate(2)),
            ],
            27,
        );
    }

    #[test]
    fn constant_folding() {
        expect_ir_result("3 * (4 + 5);", vec![Mov(PushStack, Immediate(27))], 27);
    }

    #[test]
    fn precedence() {
        expect_ir_result(
            "volatile 4 + volatile 5 * volatile 3;",
            vec![
                Mov(PushStack, Immediate(4)),
                Mov(PushStack, Immediate(5)),
                Mov(PushStack, Immediate(3)),
                Mult(StackOffset(1), StackOffset(0)),
                Add(StackOffset(2), StackOffset(1)),
                Add(SP, Immediate(2)),
            ],
            19,
        );
        expect_ir_result("4 + 5 * 3;", vec![Mov(PushStack, Immediate(19))], 19);
    }

    #[test]
    fn negation() {
        expect_ir_result("-3;", vec![Mov(PushStack, Immediate(-3))], -3);

        expect_ir_result(
            "let a := 3; -a;",
            vec![
                Mov(PushStack, Immediate(3)),
                Mov(PushStack, Immediate(0)),
                Sub(StackOffset(0), StackOffset(1)),
            ],
            -3,
        );

        expect_ir_result(
            "-(volatile 3);",
            vec![
                Mov(PushStack, Immediate(3)),
                Mov(PushStack, Immediate(0)),
                Sub(StackOffset(0), StackOffset(1)),
                Mov(StackOffset(1), StackOffset(0)),
                Add(SP, Immediate(1)),
            ],
            -3,
        );
    }

    #[test]
    fn ref_deref() {
        expect_result(
            "
            let x := 1;
            let ptr := &x;
            (volatile ptr)[];
        ",
            vec![
                // // let x := 1;
                // Mov(PushStack, Immediate(1)),
                // // let ptr := &x;
                // LoadAddress(PushStack, StackOffset(0)),
                // // volatile ptr
                // Mov(PushStack, StackOffset(0)),
                // // []
                // Mov(R0, StackOffset(0)),
                // Mov(StackOffset(0), R0Offset(0)),
            ],
            1,
        );

        expect_ir_result(
            "
            let x := 1;
            let ptr := &x;
            ptr[];
        ",
            vec![
                // let x := 1;
                Mov(PushStack, Immediate(1)),
                // let ptr := &x;
                LoadAddress(PushStack, StackOffset(0)),
                // ptr[]
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
            x;
        ",
            vec![
                // let x := 1;
                Mov(PushStack, Immediate(1)),
                // let ptr := &x;
                LoadAddress(PushStack, StackOffset(0)),
                // ptr[] := 2;
                Mov(R0, StackOffset(0)),
                Mov(R0Offset(0), Immediate(2)),
                // x
                Mov(PushStack, StackOffset(1)),
            ],
            2,
        );
    }

    #[test]
    fn type_casting() {
        expect_ir_result(
            "(volatile true as Int) +  2;",
            vec![
                Mov(PushStack, Immediate(1)),
                Add(StackOffset(0), Immediate(2)),
            ],
            3,
        );
    }

    #[test]
    fn type_alias() {
        expect_ir(
            "
                type Word := Int;
                let a : Word := 3;
            ",
            vec![Mov(PushStack, Immediate(3))],
        )
    }

    #[test]
    fn record() {
        expect_result(
            "
            type Point := record { x: Int, y: Int };
            let p := Point { x: 123, y: 456 };
            (volatile p).x;
        ",
            vec![
                // // let p := Point { x: 1, y: 2 };
                // Sub(SP, Immediate(2)),
                // Mov(StackOffset(0), Immediate(123)),
                // Mov(StackOffset(1), Immediate(456)),
                // // volatile p
                // Sub(SP, Immediate(2)),
                // Mov(StackOffset(0), StackOffset(2)),
                // Mov(StackOffset(1), StackOffset(3)),
                // // .x
                // Mov(StackOffset(1), StackOffset(0)),
                // Add(SP, Immediate(1)),
            ],
            123,
        );

        expect_result(
            "
            type Point := record { x: Int, y: Int };
            let p := Point { x: 123, y: 456 };
            (volatile p).y;
        ",
            vec![
                // let p := Point { x: 1, y: 2 };
                Sub(SP, Immediate(2)),
                Mov(StackOffset(0), Immediate(123)),
                Mov(StackOffset(1), Immediate(456)),
                // volatile p
                Sub(SP, Immediate(2)),
                Mov(StackOffset(0), StackOffset(2)),
                Mov(StackOffset(1), StackOffset(3)),
                // .y
                Add(SP, Immediate(1)),
            ],
            456,
        );

        expect_ir_result(
            "
            type Point := record { x: Int, y: Int };
            let p := Point { x: 123, y: 456 };
            p.x;
        ",
            vec![
                // let p := Point { x: 1, y: 2 };
                Sub(SP, Immediate(2)),
                Mov(StackOffset(0), Immediate(123)),
                Mov(StackOffset(1), Immediate(456)),
                // p.x
                Mov(PushStack, StackOffset(0)),
            ],
            123,
        );
    }

    #[test]
    fn record_assignment() {
        expect_ir(
            "
            type Point := record { x: Int, y: Int };
            let p := Point { x: 123, y: 456 };
            p.y := 789;
        ",
            vec![
                Sub(SP, Immediate(2)),
                Mov(StackOffset(0), Immediate(123)),
                Mov(StackOffset(1), Immediate(456)),
                Mov(StackOffset(1), Immediate(789)),
            ],
        );
    }

    #[test]
    fn pointer_to_record_field() {
        expect_ir_result(
            "
            type Point := record { x: Int, y: Int };
            let p := Point { x: 1, y: 2 };
            let ptr := &p.x;
            p.x := 5;
            (volatile ptr)[];
        ",
            vec![
                // let p := Point { x: 1, y: 2 };
                Sub(SP, Immediate(2)),
                Mov(StackOffset(0), Immediate(1)),
                Mov(StackOffset(1), Immediate(2)),
                // let ptr := &p.x;
                LoadAddress(PushStack, StackOffset(0)),
                // p.x := 5;
                Mov(StackOffset(1), Immediate(5)),
                // volatile ptr
                Mov(PushStack, StackOffset(0)),
                // []
                Mov(R0, StackOffset(0)),
                Mov(PushStack, R0Offset(0)),
                // cleanup
                Mov(StackOffset(1), StackOffset(0)),
                Add(SP, Immediate(1)),
            ],
            5,
        );

        expect_ir_result(
            "
            type Point := record { x: Int, y: Int };
            let p := Point { x: 1, y: 2 };
            let ptr := &p.x;
            p.x := 5;
            ptr[];
        ",
            vec![
                // let p := Point { x: 1, y: 2 };
                Sub(SP, Immediate(2)),
                Mov(StackOffset(0), Immediate(1)),
                Mov(StackOffset(1), Immediate(2)),
                // let ptr := &p.x;
                LoadAddress(PushStack, StackOffset(0)),
                // p.x := 5;
                Mov(StackOffset(1), Immediate(5)),
                // ptr[]
                Mov(R0, StackOffset(0)),
                Mov(PushStack, R0Offset(0)),
            ],
            5,
        );
    }

    #[test]
    fn deref_record_pointer_field() {
        expect_ir_result(
            "
            type Point := record { x: Int, y: Int };
            let p := Point { x: 1, y: 2 };
            let ptr := &p;
            ptr[].y;
        ",
            vec![
                // let p := Point { x: 1, y: 2 };
                Sub(SP, Immediate(2)),
                Mov(StackOffset(0), Immediate(1)),
                Mov(StackOffset(1), Immediate(2)),
                // let ptr := &p;
                LoadAddress(PushStack, StackOffset(0)),
                Mov(R0, StackOffset(0)),
                // ptr[].y
                Mov(PushStack, R0Offset(1)),
            ],
            2,
        );
    }

    #[test]
    fn oneof() {
        expect_ir_result(
            "
            type TrafficLight := oneof {Red, Yellow, Green};
            TrafficLight.Yellow;
        ",
            vec![Mov(PushStack, Immediate(1))],
            1,
        )
    }

    #[test]
    fn bitset() {
        expect_ir_result(
            "
            type Flags := oneof {Carry, Overflow, Zero, Negative, Extend};
            let flags := Flags{Carry, Zero};
            flags.Zero;
        ",
            vec![
                Mov(PushStack, Immediate(0b101)),
                Mov(PushStack, StackOffset(0)),
                BitTest(StackOffset(0), Immediate(2)),
                Mov(StackOffset(0), R0),
            ],
            1,
        );
    }

    #[test]
    fn array() {
        expect_result(
            "
                let xs := array[Int: 4]{10, 20, 30, 40};
                xs[volatile 1];
            ",
            vec![
                // let xs := array[Int: 4]{10, 20, 30, 40};
                Sub(SP, Immediate(4)),
                Mov(StackOffset(0), Immediate(10)),
                Mov(StackOffset(1), Immediate(20)),
                Mov(StackOffset(2), Immediate(30)),
                Mov(StackOffset(3), Immediate(40)),
                // &xs + (1 * sizeof Int)
                LoadAddress(PushStack, StackOffset(0)),
                Mov(PushStack, Immediate(1)),
                Mult(StackOffset(0), Immediate(1)),
                Add(StackOffset(1), StackOffset(0)),
                // deref
                Mov(R0, StackOffset(1)),
                Mov(StackOffset(1), R0Offset(0)),
                // cleanup
                Add(SP, Immediate(1)),
            ],
            20,
        );

        expect_result(
            "
                let xs := array[Int: 4]{10, 20, 30, 40};
                xs[1];
            ",
            vec![
                // let xs := array[Int: 4]{10, 20, 30, 40};
                Sub(SP, Immediate(4)),
                Mov(StackOffset(0), Immediate(10)),
                Mov(StackOffset(1), Immediate(20)),
                Mov(StackOffset(2), Immediate(30)),
                Mov(StackOffset(3), Immediate(40)),
                // &xs + 1
                LoadAddress(PushStack, StackOffset(0)),
                Add(StackOffset(0), Immediate(1)),
                // deref
                Mov(R0, StackOffset(0)),
                Mov(StackOffset(0), R0Offset(0)),
            ],
            20,
        );
    }

    #[test]
    fn variants() {
        expect_result(
            "
                type Point := record { x: Int, y: Int };
                type Shape := record {
                    fill: Bool,
                    case Rect {
                        top_left: Point,
                        bottom_right: Point,
                    }
                    case Circle {
                        center: Point,
                        radius: Int
                    }
                };
                
                let disc := Shape.Circle{
                    fill: true,
                    center: Point{x: 0, y: 1},
                    radius: 3
                };

                disc.fill;
            ",
            vec![],
            1,
        );
    }

    #[test]
    fn if_stmt() {
        expect_ir_result(
            "
            let i := 1;
            if false then
                i := 3;
            end
            i;
        ",
            vec![
                // let i := 1;
                Mov(PushStack, Immediate(1)),
                // false
                Mov(PushStack, Immediate(0)),
                // if .. then
                BranchZero(Immediate(1), PopStack),
                // i := 3
                Mov(StackOffset(0), Immediate(3)),
                // i;
                Mov(PushStack, StackOffset(0)),
            ],
            1,
        );
        expect_result(
            "
            let i := 1;
            if true then
                i := 3;
            end
            i;
        ",
            vec![],
            3,
        );
    }

    #[test]
    fn if_else_stmt() {
        expect_ir_result(
            "
            let i := 0;
            if true then
                i := 3;
            else
                i := 4;
            end
            i;
        ",
            vec![
                // let i := 0
                Mov(PushStack, Immediate(0)),
                // true
                Mov(PushStack, Immediate(1)),
                // if .. then
                BranchZero(Immediate(2), PopStack),
                // i := 3
                Mov(StackOffset(0), Immediate(3)),
                // -> skip else
                BranchZero(Immediate(1), Immediate(0)),
                // else:
                // i := 4;
                Mov(StackOffset(0), Immediate(4)),
                // i
                Mov(PushStack, StackOffset(0)),
            ],
            3,
        );
        expect_result(
            "
            let i := 0;
            if false then
                i := 3;
            else
                i := 4;
            end
            i;
        ",
            vec![],
            4,
        );
    }

    #[test]
    fn else_if() {
        expect_ir_result(
            "
            let i := 0;
            if false then
                i := 3;
            else if true then
                i := 4;
            else 
                i := 5;
            end
            i;
        ",
            vec![
                // let i := 0;
                Mov(PushStack, Immediate(0)),
                // false
                Mov(PushStack, Immediate(0)),
                // if .. then
                BranchZero(Immediate(2), PopStack),
                // i := 3
                Mov(StackOffset(0), Immediate(3)),
                // -> end
                BranchZero(Immediate(5), Immediate(0)),
                // true
                Mov(PushStack, Immediate(1)),
                // if .. then
                BranchZero(Immediate(2), PopStack),
                // i := 4
                Mov(StackOffset(0), Immediate(4)),
                //  -> end
                BranchZero(Immediate(1), Immediate(0)),
                // i := 5
                Mov(StackOffset(0), Immediate(5)),
                // end: i
                Mov(PushStack, StackOffset(0)),
            ],
            4,
        );
    }

    #[test]
    fn while_stmt() {
        expect_ir_result(
            "
            let count := 0;
            while count != 10 loop
                count := count + 1;
            end
            count;
        ",
            vec![
                // let count := 0;
                Mov(PushStack, Immediate(0)),
                // begin: count
                Mov(PushStack, StackOffset(0)),
                // != 10
                NotEqual(StackOffset(0), Immediate(10)),
                // -> end
                BranchZero(Immediate(5), PopStack),
                // count
                Mov(PushStack, StackOffset(0)),
                // + 1
                Add(StackOffset(0), Immediate(1)),
                // count :=
                Mov(StackOffset(1), StackOffset(0)),
                // drop
                Add(SP, Immediate(1)),
                // -> begin
                BranchZero(Immediate(-8), Immediate(0)),
                // end: count
                Mov(PushStack, StackOffset(0)),
            ],
            10,
        );
    }

    #[test]
    fn match_stmt() {
        expect_ir_result(
            "
            type Option := record {
                case Some {
                    value: Int
                }
                case None {}
            };

            let result := 1;
            let opt := Option.Some{value: 3};

            match opt then
            case Some{value}:
                result := value;
            case None{}:
                result := 10;
            end

            result;
        ",
            vec![
                // let result := 0;
                Mov(PushStack, Immediate(1)),
                // let opt := Option.Some{value: 3};
                Sub(SP, Immediate(2)),
                Mov(StackOffset(0), Immediate(0)),
                Mov(StackOffset(1), Immediate(3)),
                // opt (todo: don't need to force resolution)
                Sub(SP, Immediate(2)),
                Mov(StackOffset(0), StackOffset(2)),
                Mov(StackOffset(1), StackOffset(3)),
                // match .. then
                BranchZero(StackOffset(0), Immediate(0)),
                BranchZero(Immediate(1), Immediate(0)),
                BranchZero(Immediate(2), Immediate(0)),
                // Some: result := value
                Mov(StackOffset(4), StackOffset(1)),
                BranchZero(Immediate(2), Immediate(0)),
                // None: result := 10
                Mov(StackOffset(4), Immediate(10)),
                BranchZero(Immediate(0), Immediate(0)),
                // end; result
                Mov(PushStack, StackOffset(4)),
                // cleanup
                Mov(StackOffset(2), StackOffset(0)),
                Add(SP, Immediate(2)),
            ],
            3,
        );

        expect_ir_result(
            "
            type Option := record {
                case Some {
                    value: Int
                }
                case None {}
            };

            let result := 1;
            let opt := Option.None{};

            match opt then
            case Some{value}:
                result := value;
            case None{}:
                result := 10;
            end

            result;
        ",
            vec![
                // let result := 0;
                Mov(PushStack, Immediate(1)),
                // let opt := Option.None{};
                Sub(SP, Immediate(2)),
                Mov(StackOffset(0), Immediate(1)),
                // opt (todo: don't need to force resolution)
                Sub(SP, Immediate(2)),
                Mov(StackOffset(0), StackOffset(2)),
                Mov(StackOffset(1), StackOffset(3)),
                // match .. then
                BranchZero(StackOffset(0), Immediate(0)),
                BranchZero(Immediate(1), Immediate(0)),
                BranchZero(Immediate(2), Immediate(0)),
                // Some: result := value
                Mov(StackOffset(4), StackOffset(1)),
                BranchZero(Immediate(2), Immediate(0)),
                // None: result := 10
                Mov(StackOffset(4), Immediate(10)),
                BranchZero(Immediate(0), Immediate(0)),
                // end; result
                Mov(PushStack, StackOffset(4)),
                // cleanup
                Mov(StackOffset(2), StackOffset(0)),
                Add(SP, Immediate(2)),
            ],
            10,
        );
    }

    #[test]
    fn block_scope() {
        expect_ir_result(
            "
            let x := 1;
            if true then
                let x := 2;
                x := 3;
            end
            x;
        ",
            vec![
                // let x:= 1
                Mov(PushStack, Immediate(1)),
                // true
                Mov(PushStack, Immediate(1)),
                // if .. then
                BranchZero(Immediate(3), PopStack),
                // let x := 2; (new x)
                Mov(PushStack, Immediate(2)),
                // x := 3;
                Mov(StackOffset(0), Immediate(3)),
                // drop scope
                Add(SP, Immediate(1)),
                // x
                Mov(PushStack, StackOffset(0)),
            ],
            1,
        );
    }

    #[test]
    fn subroutine() {
        expect_program(
            "
            type Option := record {
                case Some {
                    value: Int
                }
                case None {}
            };

            sub main() do
                let x := Option.Some{value: 3};
                return;
            end
        ",
            vec![
                // let x := Option.Some{value: 3};
                Sub(SP, Immediate(2)),
                Mov(StackOffset(0), Immediate(0)),
                Mov(StackOffset(1), Immediate(3)),
                // drop & return
                Add(SP, Immediate(2)),
                Return,
            ],
        );
    }
}
