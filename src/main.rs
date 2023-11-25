mod block;
mod compiler;
mod expr;
mod lexer;
mod memory;
mod op;
mod parser;
mod runtime;
mod ty;

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
    use super::{IRCond::*, Register::*, EA::*, IR::*};

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
        expect_ir_result("123;", vec![Mov(PreDec(SP), Immediate(123))], 123);
    }

    #[test]
    fn whitespace() {
        expect_ir(
            "
            123;

            ",
            vec![Mov(PreDec(SP), Immediate(123))],
        )
    }

    #[test]
    fn comments() {
        expect_ir(
            "123; # This is a comment",
            vec![Mov(PreDec(SP), Immediate(123))],
        )
    }

    #[test]
    fn unexpected_char() {
        expect_err(" £ ", UnexpectedChar)
    }

    #[test]
    fn bools() {
        expect_ir("true;", vec![Mov(PreDec(SP), Immediate(1))]);
        expect_ir("false;", vec![Mov(PreDec(SP), Immediate(0))]);
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
                Mov(PreDec(SP), Immediate(1)),
                Mov(PreDec(SP), Immediate(2)),
                Mov(PreDec(SP), Offset(SP, 1)),
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
            vec![Mov(PreDec(SP), Immediate(1))],
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
                Mov(PreDec(SP), Immediate(1)),
                Mov(PreDec(SP), Immediate(3)),
                Mov(Offset(SP, 1), Offset(SP, 0)),
                Mov(PreDec(SP), Offset(SP, 1)),
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
                Mov(PreDec(SP), Immediate(1)),     // [x: 1]
                Mov(PreDec(SP), Immediate(2)),     // [y: 2, x: 1]
                Mov(PreDec(SP), Offset(SP, 1)),    // [1, y: 2, x: 1]
                Add(Offset(SP, 0), Immediate(3)),  // [4, y: 2, x: 1]
                Add(Offset(SP, 0), Offset(SP, 1)), // [6, y: 2, x: 1]
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
                Mov(PreDec(SP), Immediate(3)),
                Mov(PreDec(SP), Immediate(4)),
                Mov(PreDec(SP), Immediate(5)),
                Add(Offset(SP, 1), Offset(SP, 0)),
                Mult(Offset(SP, 2), Offset(SP, 1)),
                Add(Register(SP), Immediate(2)),
            ],
            27,
        );
    }

    #[test]
    fn constant_folding() {
        expect_ir_result("3 * (4 + 5);", vec![Mov(PreDec(SP), Immediate(27))], 27);
    }

    #[test]
    fn precedence() {
        expect_ir_result(
            "volatile 4 + volatile 5 * volatile 3;",
            vec![
                Mov(PreDec(SP), Immediate(4)),
                Mov(PreDec(SP), Immediate(5)),
                Mov(PreDec(SP), Immediate(3)),
                Mult(Offset(SP, 1), Offset(SP, 0)),
                Add(Offset(SP, 2), Offset(SP, 1)),
                Add(Register(SP), Immediate(2)),
            ],
            19,
        );
        expect_ir_result("4 + 5 * 3;", vec![Mov(PreDec(SP), Immediate(19))], 19);
    }

    #[test]
    fn negation() {
        expect_ir_result("-3;", vec![Mov(PreDec(SP), Immediate(-3))], -3);

        expect_ir_result(
            "let a := 3; -a;",
            vec![
                Mov(PreDec(SP), Immediate(3)),
                Mov(PreDec(SP), Immediate(0)),
                Sub(Offset(SP, 0), Offset(SP, 1)),
            ],
            -3,
        );

        expect_ir_result(
            "-(volatile 3);",
            vec![
                Mov(PreDec(SP), Immediate(3)),
                Mov(PreDec(SP), Immediate(0)),
                Sub(Offset(SP, 0), Offset(SP, 1)),
                Mov(Offset(SP, 1), Offset(SP, 0)),
                Add(Register(SP), Immediate(1)),
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
                // Mov(PreDec(SP), Immediate(1)),
                // // let ptr := &x;
                // LoadAddress(PreDec(SP), Offset(SP, 0)),
                // // volatile ptr
                // Mov(PreDec(SP), Offset(SP, 0)),
                // // []
                // Mov(Register(R0), Offset(SP, 0)),
                // Mov(Offset(SP, 0), Offset(R0, 0)),
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
                Mov(PreDec(SP), Immediate(1)),
                // let ptr := &x;
                LoadAddress(PreDec(SP), Offset(SP, 0)),
                // ptr[]
                Mov(Register(R0), Offset(SP, 0)),
                Mov(PreDec(SP), Offset(R0, 0)),
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
                Mov(PreDec(SP), Immediate(1)),
                // let ptr := &x;
                LoadAddress(PreDec(SP), Offset(SP, 0)),
                // ptr[] := 2;
                Mov(Register(R0), Offset(SP, 0)),
                Mov(Offset(R0, 0), Immediate(2)),
                // x
                Mov(PreDec(SP), Offset(SP, 1)),
            ],
            2,
        );
    }

    #[test]
    fn type_casting() {
        expect_ir_result(
            "(volatile true as Int) +  2;",
            vec![
                Mov(PreDec(SP), Immediate(1)),
                Add(Offset(SP, 0), Immediate(2)),
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
            vec![Mov(PreDec(SP), Immediate(3))],
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
                // Sub(Register(SP), Immediate(2)),
                // Mov(Offset(SP, 0), Immediate(123)),
                // Mov(Offset(SP, 1), Immediate(456)),
                // // volatile p
                // Sub(Register(SP), Immediate(2)),
                // Mov(Offset(SP, 0), Offset(SP, 2)),
                // Mov(Offset(SP, 1), Offset(SP, 3)),
                // // .x
                // Mov(Offset(SP, 1), Offset(SP, 0)),
                // Add(Register(SP), Immediate(1)),
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
                Sub(Register(SP), Immediate(2)),
                Mov(Offset(SP, 0), Immediate(123)),
                Mov(Offset(SP, 1), Immediate(456)),
                // volatile p
                Sub(Register(SP), Immediate(2)),
                Mov(Offset(SP, 0), Offset(SP, 2)),
                Mov(Offset(SP, 1), Offset(SP, 3)),
                // .y
                Add(Register(SP), Immediate(1)),
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
                Sub(Register(SP), Immediate(2)),
                Mov(Offset(SP, 0), Immediate(123)),
                Mov(Offset(SP, 1), Immediate(456)),
                // p.x
                Mov(PreDec(SP), Offset(SP, 0)),
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
                Sub(Register(SP), Immediate(2)),
                Mov(Offset(SP, 0), Immediate(123)),
                Mov(Offset(SP, 1), Immediate(456)),
                Mov(Offset(SP, 1), Immediate(789)),
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
                Sub(Register(SP), Immediate(2)),
                Mov(Offset(SP, 0), Immediate(1)),
                Mov(Offset(SP, 1), Immediate(2)),
                // let ptr := &p.x;
                LoadAddress(PreDec(SP), Offset(SP, 0)),
                // p.x := 5;
                Mov(Offset(SP, 1), Immediate(5)),
                // volatile ptr
                Mov(PreDec(SP), Offset(SP, 0)),
                // []
                Mov(Register(R0), Offset(SP, 0)),
                Mov(PreDec(SP), Offset(R0, 0)),
                // cleanup
                Mov(Offset(SP, 1), Offset(SP, 0)),
                Add(Register(SP), Immediate(1)),
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
                Sub(Register(SP), Immediate(2)),
                Mov(Offset(SP, 0), Immediate(1)),
                Mov(Offset(SP, 1), Immediate(2)),
                // let ptr := &p.x;
                LoadAddress(PreDec(SP), Offset(SP, 0)),
                // p.x := 5;
                Mov(Offset(SP, 1), Immediate(5)),
                // ptr[]
                Mov(Register(R0), Offset(SP, 0)),
                Mov(PreDec(SP), Offset(R0, 0)),
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
                Sub(Register(SP), Immediate(2)),
                Mov(Offset(SP, 0), Immediate(1)),
                Mov(Offset(SP, 1), Immediate(2)),
                // let ptr := &p;
                LoadAddress(PreDec(SP), Offset(SP, 0)),
                Mov(Register(R0), Offset(SP, 0)),
                // ptr[].y
                Mov(PreDec(SP), Offset(R0, 1)),
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
            vec![Mov(PreDec(SP), Immediate(1))],
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
                Mov(PreDec(SP), Immediate(0b101)),
                Mov(PreDec(SP), Offset(SP, 0)),
                BitTest(Offset(SP, 0), Immediate(2)),
                SetIf(Offset(SP, 0), NotZero),
            ],
            1,
        );
    }

    #[test]
    fn array() {
        expect_ir_result(
            "
                let xs := array[Int: 4]{10, 20, 30, 40};
                xs[volatile 1];
            ",
            vec![
                //
                Sub(Register(SP), Immediate(4)),
                Mov(Offset(SP, 0), Immediate(10)),
                Mov(Offset(SP, 1), Immediate(20)),
                Mov(Offset(SP, 2), Immediate(30)),
                Mov(Offset(SP, 3), Immediate(40)),
                // xs
                LoadAddress(PreDec(SP), Offset(SP, 0)),
                // [1]
                Mov(PreDec(SP), Immediate(1)),
                Mult(Offset(SP, 0), Immediate(1)),
                Add(Offset(SP, 1), Offset(SP, 0)),
                Mov(Register(R0), Offset(SP, 1)),
                Mov(PreDec(SP), Offset(R0, 0)),
                // compact
                Mov(Offset(SP, 2), Offset(SP, 0)),
                Add(Register(SP), Immediate(2)),
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
                Sub(Register(SP), Immediate(4)),
                Mov(Offset(SP, 0), Immediate(10)),
                Mov(Offset(SP, 1), Immediate(20)),
                Mov(Offset(SP, 2), Immediate(30)),
                Mov(Offset(SP, 3), Immediate(40)),
                // &xs + 1
                LoadAddress(PreDec(SP), Offset(SP, 0)),
                Add(Offset(SP, 0), Immediate(1)),
                // deref
                Mov(Register(R0), Offset(SP, 0)),
                Mov(Offset(SP, 0), Offset(R0, 0)),
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
                Mov(PreDec(SP), Immediate(1)),
                // if false then
                BranchIf(Immediate(1), Always),
                // i := 3
                Mov(Offset(SP, 0), Immediate(3)),
                // i;
                Mov(PreDec(SP), Offset(SP, 0)),
            ],
            1,
        );
        expect_ir_result(
            "
            let i := 1;
            if true then
                i := 3;
            end
            i;
        ",
            vec![
                // let i := 1;
                Mov(PreDec(SP), Immediate(1)),
                // if true then
                BranchIf(Immediate(1), Never),
                // i := 3
                Mov(Offset(SP, 0), Immediate(3)),
                // i;
                Mov(PreDec(SP), Offset(SP, 0)),
            ],
            3,
        );
        expect_ir_result(
            "
            let i := 1;
            let cond := true;
            if cond then
                i := 3;
            end
            i;
        ",
            vec![
                // let i := 1;
                Mov(PreDec(SP), Immediate(1)),
                // let cond := true;
                Mov(PreDec(SP), Immediate(1)),
                // cond
                Mov(PreDec(SP), Offset(SP, 0)),
                Cmp(PostInc(SP), Immediate(1)),
                // if ... then
                BranchIf(Immediate(1), Zero),
                // i := 3
                Mov(Offset(SP, 1), Immediate(3)),
                // i;
                Mov(PreDec(SP), Offset(SP, 1)),
            ],
            3,
        );
    }

    #[test]
    fn if_else_stmt() {
        expect_ir_result(
            "
            let i := 0;
            let cond := true;
            if cond then
                i := 3;
            else
                i := 4;
            end
            i;
        ",
            vec![
                // let i := 1;
                Mov(PreDec(SP), Immediate(0)),
                // let cond := true;
                Mov(PreDec(SP), Immediate(1)),
                // if cond
                Mov(PreDec(SP), Offset(SP, 0)),
                Cmp(PostInc(SP), Immediate(1)),
                // then
                BranchIf(Immediate(2), Zero),
                // i := 3
                Mov(Offset(SP, 1), Immediate(3)),
                BranchIf(Immediate(1), Always),
                // else: i := 4
                Mov(Offset(SP, 1), Immediate(4)),
                // i;
                Mov(PreDec(SP), Offset(SP, 1)),
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
            let cmp := 20;
            if cmp = 10 then
                i := 3;
            else if cmp = 20 then
                i := 4;
            else 
                i := 5;
            end
            i;
        ",
            vec![
                // let i := 0;
                Mov(PreDec(SP), Immediate(0)),
                // let cmp := 20;
                Mov(PreDec(SP), Immediate(20)),
                // if cmp = 10
                Mov(PreDec(SP), Offset(SP, 0)),
                Cmp(PostInc(SP), Immediate(10)),
                // then
                BranchIf(Immediate(2), Zero),
                // i := 3
                Mov(Offset(SP, 1), Immediate(3)),
                // -> end
                BranchIf(Immediate(6), Always),
                // else if cmp = 20
                Mov(PreDec(SP), Offset(SP, 0)),
                Cmp(PostInc(SP), Immediate(20)),
                // then
                BranchIf(Immediate(2), Zero),
                // i := 4
                Mov(Offset(SP, 1), Immediate(4)),
                // -> end
                BranchIf(Immediate(1), Always),
                // else i := 5
                Mov(Offset(SP, 1), Immediate(5)),
                // end: i
                Mov(PreDec(SP), Offset(SP, 1)),
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
                // let count := 0
                Mov(PreDec(SP), Immediate(0)),
                // begin: count != 10
                Mov(PreDec(SP), Offset(SP, 0)),
                Cmp(PostInc(SP), Immediate(10)),
                // while .. loop
                BranchIf(Immediate(5), NotZero),
                // count
                Mov(PreDec(SP), Offset(SP, 0)),
                // + 1
                Add(Offset(SP, 0), Immediate(1)),
                // count := _
                Mov(Offset(SP, 1), Offset(SP, 0)),
                // end
                Add(Register(SP), Immediate(1)),
                // -> begin
                BranchIf(Immediate(-8), Always),
                // count
                Mov(PreDec(SP), Offset(SP, 0)),
            ],
            10,
        );
    }

    #[test]
    fn match_stmt() {
        expect_result(
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
                Mov(PreDec(SP), Immediate(1)),
                // let opt := Option.Some{value: 3};
                Sub(Register(SP), Immediate(2)),
                Mov(Offset(SP, 0), Immediate(0)),
                Mov(Offset(SP, 1), Immediate(3)),
                // opt (todo: don't need to force resolution)
                Sub(Register(SP), Immediate(2)),
                Mov(Offset(SP, 0), Offset(SP, 2)),
                Mov(Offset(SP, 1), Offset(SP, 3)),
                // match .. then
                BranchIf(Offset(SP, 0), Always),
                BranchIf(Immediate(1), Always),
                BranchIf(Immediate(2), Always),
                // Some: result := value
                Mov(Offset(SP, 4), Offset(SP, 1)),
                BranchIf(Immediate(2), Always),
                // None: result := 10
                Mov(Offset(SP, 4), Immediate(10)),
                BranchIf(Immediate(0), Always),
                // end; result
                Mov(PreDec(SP), Offset(SP, 4)),
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
                Mov(PreDec(SP), Immediate(1)),
                // let opt := Option.None{};
                Sub(Register(SP), Immediate(2)),
                Mov(Offset(SP, 0), Immediate(1)),
                // opt (todo: don't need to force resolution)
                Sub(Register(SP), Immediate(2)),
                Mov(Offset(SP, 0), Offset(SP, 2)),
                Mov(Offset(SP, 1), Offset(SP, 3)),
                // match .. then
                BranchIf(Offset(SP, 0), Always),
                BranchIf(Immediate(1), Always),
                BranchIf(Immediate(2), Always),
                // Some: result := value
                Mov(Offset(SP, 4), Offset(SP, 1)),
                BranchIf(Immediate(2), Always),
                // None: result := 10
                Mov(Offset(SP, 4), Immediate(10)),
                BranchIf(Immediate(0), Always),
                // end; result
                Mov(PreDec(SP), Offset(SP, 4)),
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
                Mov(PreDec(SP), Immediate(1)),
                // if .. then
                BranchIf(Immediate(3), Never),
                // let x := 2; (new x)
                Mov(PreDec(SP), Immediate(2)),
                // x := 3;
                Mov(Offset(SP, 0), Immediate(3)),
                // drop scope
                Add(Register(SP), Immediate(1)),
                // x
                Mov(PreDec(SP), Offset(SP, 0)),
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
                Sub(Register(SP), Immediate(2)),
                Mov(Offset(SP, 0), Immediate(0)),
                Mov(Offset(SP, 1), Immediate(3)),
                // drop & return
                Add(Register(SP), Immediate(2)),
                Return,
            ],
        );
    }

    #[test]
    fn subroutine_calls() {
        expect_program(
            "
            sub add(left: Int, right: Int) -> Int do
                return left + right;
            end

            sub main() do
                let result := add(1, 2);
                if result != 3 then
                    panic;
                end
            end

        ",
            vec![
                // add(Int, Int) -> Int
                // left
                Mov(PreDec(SP), Offset(SP, 2)),
                // + right
                Add(Offset(SP, 0), Offset(SP, 2)),
                // return :=
                Mov(Offset(SP, 4), Offset(SP, 0)),
                // return
                Add(Register(SP), Immediate(1)),
                Return,
                // main()
                // let result := add(
                Sub(Register(SP), Immediate(1)),
                // 1
                Mov(PreDec(SP), Immediate(1)),
                // 2
                Mov(PreDec(SP), Immediate(2)),
                // );
                Call(0),
                Add(Register(SP), Immediate(2)),
                // if result
                Mov(PreDec(SP), Offset(SP, 0)),
                // != 3
                Cmp(PostInc(SP), Immediate(3)),
                // then
                BranchIf(Immediate(1), NotZero),
                Panic,
                // end
                Add(Register(SP), Immediate(1)),
                Return,
            ],
        )
    }
}
