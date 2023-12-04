# Gremlin

A low level programming language

- C-like feature set, but with some modern conveniences (e.g. pattern-matching, local type inference, parameterized types)
- single-pass compiler, a la Turbo Pascal
- targets a virtual machine with a 68k-ish instruction set

Current status: a bunch of different fragmentary implementations

# Language overview

## Comments

Comments are a single line and start with a "#".

`# this is a comment`

## Identifiers

Identifiers start with a lowercase letter and can contain letters (of any case), numbers and underscores.

Type identifiers start with an uppercase letter.

## Bindings & assignment

`const` is a compile-time only value.

```
const foo := 123;
# consts can have complex expressions assigned to them, including those with references to other consts
const bar := foo * 4;
# const cannot be reassigned
bar := 6; # compile error
```

`let` is a runtime value. let-bound values can be changed in their own scope, but not as a subroutine parameter.

```
let a := 123;
# let can be assigned to a new value
a := 456;
# let _cannot_ be used as a &var reference
add_2(&var a); # type error
```

`var` is a runtime value that can be changed as a function parameter.

```
var a := 123;
add_2(&var a); # => a == 125
```

### undefined

`let a := undefined` creates binding which can only be read after assignment, e.g.

```
let a := undefined;
if foo > 1 then
  a := 2;
else
  a := 3;
end
```

## Numbers

Numbers are signed 32-bit integers. Literals can have underscores.

`123` `0xDEAD_BEEF` `0b1100_1001`

Supported operators: arithmetic, equality, comparison

Floating-point numbers are not natively supported.

## Bools

Bools are also 32 bits wide, though they can be packed densely in records (see "bitsets").

`true` `false`

Supported operators: logic, equality

## Tags

Char tags are a group of up to 4 ASCII-encoded chars, wrapped in single quotes. These are implemented as packed 4-character strings. Absent characters are nulls.

`'CODE'` is equivalent to `0x434F_4445`
`'OK'` is equivalent to `0x4F4B_0000`

Supported operators: equality

## Strings

String literals are double-quoted.

`"Hello, world!"`

String values are implemented slices into byte arrays.

Supported operators: none (use string subroutines)

## Oneof

Oneof is an enumeration of values.

```
type TrafficLight := oneof {Red, Yellow, Green};
let a := TrafficLight.Yellow;
TrafficLight.Red as Int; # => 0
TrafficLight.Green as Int; # => 2
```

Supported operators: equality

## Bitset

a `oneof` with 32 members or fewer can also be used as a bitset.

```
type TrafficLight := oneof {Red, Yellow, Green};
let can_drive :=  TrafficLight{Yellow, Green};
can_drive as Int; # => 0b0110
can_drive[TrafficLight.Red]; # => false
!can_drive; # => TrafficLight{Red}
can_drive >= TrafficLight{Yellow}; # => true
```

Cast integer to bitset for bitwise operations:

```
let bits := 0b1001 as Int{}; # => Int{0,3}
bits[0]; # => true
bits & (0b1000 as Int{}); # => Int{3}
```

Supported operators: logic, equality, comparison (as sub/superset)

## Record

A record is a heterogeneous key-value structure.

```
type Point := record {
  x: Int,
  y: Int,
};
let p := Point{x: 1, y: 2};
p.x # => 1
```

Records can have cases.

```
type Expr := record {
    source: Int,
case Number:
    value: Int,
case Add:
    left: &Expr,
    right: &Expr,
};
let equation := Expr.Add{
  source: 0,
  left: &Expr.Number{source: 1, value: 1},
  right: &Expr.Number{source: 3, value: 2},
};

# shared fields are' always accessible:
equation.source; # => 0

# case-specific fields are' not:
equation.left; # => type error

# the record also has a case field, which can be used in oneof and bitset contexts:
equation.case; # => Expr.Add
Expr{Number, Add}[equation.case] # => true

# each case has its own type that the record can be cast to:
(equation as Expr.Add).left;

# or accessed through pattern matching
match equation then
case Expr.Add:
  recur(equation.left)
case Expr.Int:
  equation.value
end
```

## Control flow

### If

```
if x then
  y();
else if z then
  w();
else
  q();
end
```

### While / loop

```
while a < 10 loop
  a := a + 1;
end
```

### Match

On primitive values, match operates by equality

```
match num then
case 1, 2:
  # ...
case 3:
  # ...
default:
  # ...
end
```

On records, match operates on case field, and must cover every case (or include `default`):

```
match equation then
case Expr.Add:
  equation.left
default:
  return;
end
```

## Arrays & Slices

Arrays are fixed-size homogeneous collections.

```
let xs := array[Int: 4]{10,20,30,40};
xs[1]; #=> 20
```

Slices are variable-sized views into arrays.

## Subroutines

```
sub add(l: Point, r: Point) -> Point do
  return Point {x: l.x + r.x, y: l.y + r.y };
end

add(Point{x: 1, y: 2}, Point{x: 3, y: 4});
```

### chained calls

`l..add(r)`

### Nested functions

There's no implicit heap allocation in this language, so closures will require more ceremony than in other languages -- e.g. a subroutine that takes a "callback" would also need to take a pointer to the callback's context value

The context object could be stack-allocated as long as the nested function doesn't escape its original context

Maybe something like

```
let x := 1;
let y := 2;
call_foo(sub {x, y}(arg) do
  return x + y * arg;
end);
```

would desugar to

```
let x := 1;
let y := 2;
type Ctx_anon := record {x: Int, y: Int};
sub anon (ctx: Ctx_anon, arg: Int) -> Int do
  return ctx.x + ctx.y * arg;
end
call_foo(Ctx_anon{x: x, y: y}, anon);
```

## References

Regular references are read-only, and can be made from any expression:

```
let a := 123;
let b := &a;
b[]; # => 123
b[] := 456; # => error
```

Var references are mutable, and can only be made from var bindings:

```
var a := 123;
let b := &var a;
b[]; # => 123
b[] := 456; # ok
a # => 456
```

Note that the language will do nothing to stop you from using an invalid reference!

TODO: do var references automatically propagate thru records? Like if you have &var Expr do you also have &var Expr.Add.left? I think probably no.
TODO: can you cast &Int to &var Int? Probably yes, but maybe it should be extra-discouraged, like wrong-size casting

## Type-casting

Types can be cast to other types of the same size using `as`;

```
let x := true;
let y := x as Int; # => 1
```

You can cast values to pointers for arbitrary memory access, if you insist:

```
var addr := 0x0F as &var Int;
addr[] := 123; # poke 123 into memory
```

Or force a cast into the wrong size:

```
let p := Point{x: 1, y: 2 };
let x := (&p as &Int)[];
```

## Heap-allocated values

I haven't figured this part out yet. I think it will resemble Zig's explicit allocator / `defer allocator.free(item)` pattern

## Parameterized types

Gremlin supports parameterized types, aka "generics".

```
type List[T] := record {
case Empty:
case Cons:
    value: T,
    next: &List[T]
}
```

### Type inference

`let x := List.Cons{...}` infers the type from the value, but `let x := List.Empty` is ambiguous. Needs `let x : List[Int] := List.Empty` or `let x := List[Int].Empty`;

### Type parameters in subroutines

This needs to be supported in subroutines to be useful, but this must add necessary overhead. I'm not sure exactly how implicit or explicit this should be.

```
sub[T] push_item(value: T, list: &List[T]) do
  let out := List.Cons{value: value, next: list[]};
  list[] := out;
end

# functions something like
sub push_item(
  sizeof_T: Int,  # size is passed as additional parameter
  value: &Void,   # non-pointer args using parameterized type sent as pointer
  var list: &List[Void]
) do
  # NOTE: variable sized stack allocation not user-accessible, for demonstration purposes only
  var out := alloca(sizeof List.case + sizeof_T + sizeof  &List[Void]);
  write(&var out, 0, sizeof List.case, &List.Cons);
  write(&var out, sizeof List.case, sizeof_T, value);
  write(&var out, sizeof List.case + sizeof_T, sizeof  &List[Void], list);
  list[] := out;
end
```

If a function only uses _pointers_ to type params, (easy) and doesnt try to do anything useful with said pointer (less easy), most of this can be skipped.

## Error handling

Basically do what Rust does, but with `?` operator hard-coded to work with `Result[T,E]`

---

# homermobile zone

need to figure out what exactly the deal with syntax is, choose level of affectedness that is odd to look at but easy to type

## coroutines

```
sub add(l: Point, r: Point): Point do
  return Point {x := l.x + r.x; y := l.y + r.y }
end

co alternate_and_count(x: int, y: int) yields int do
  let count := 0
  loop
    yield x
    yield y
    yield count
    count := count + 1
  end
end
```

coroutine and closure need an allocator

## trait-ish things

define an abstract type

```
type Writer[T] := struct
  write_byte: sub (writer: &T, data: int)
end
```

create a default implementation for a concrete type

```
static impl Writer[File] := Writer{
  write_byte := sub (writer: &File, data: int) do
    # ...
  end
}
```

in a subroutine, use impl parameters

```
sub write[T](writable: &T, writer: impl Writer[T], data: Vec[int])
  for byte in iter(data) do
    writer.write_byte(writable, byte)
  end
end
```

at call site, use underscore to use implicit

```
write(&file, _, data)
# or override with explicit param
write(&file, &custom_file_writer, data)
```

impls are looked up in lexical scope, so you need to import them
subroutines that use impl args can continue to propagate them as impls or use them as regular values

this is maybe also useful for "context" args

inheritance
needs to be built into the type, but theoretically possible

```
type Foo := struct
  bar: sub (int, impl Foo): int
end

# ...

impl Foo := Foo{
  bar: sub (value: int, _: impl Foo): int do
    # ...
  end
}

# ...

impl Foo := Foo{
  bar: sub (value: int, parent: impl Foo): int do
    logger(value);
    parent.sub(value, parennt)
  end
}

# ...

let result := (impl Foo).bar(3,_)

```

# modules

two levels of organization: a namespace & a package
a namespace is just a collection of related types, subroutines, values etc
each file is a namespace, and can contain nested namespaces
no enforced privacy between namespaces within the same package
no transitive imports

a collection of namespaces can be collected into a "package"
a package has a header file that defines the public interface of a package
the public interface can use namespaces or a single flat namespace
the structure of the public interface does not need to match the private structure
a package is a single compilation unit & can be loaded in & out of memory in a running program dynamically

imports look like

```
use foo.bar.baz
use foo.quux{plugh, impl Xyzzy[Foo]}
from "package" use {gleh}
```
