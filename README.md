# gremlin_rs

A low level programming language

# homermobile zone

need to figure out what exactly the deal with syntax is, choose level of affectedness that is odd to look at but easy to type

## enums & tagged variants

oneof type declaration

```
type TrafficLight := oneof Red; Yellow; Green end
```

oneof expression (a bitset)

```
let should_go := TrafficLight{Yellow;Green}
if contains(should_go, color) then ... end
```

set union, intersection, difference, complement with and, or, xor, not
check and set bits with fields

```
should_go.Yellow # true
should_go.Red := false
should_go[value]
```

tagged variants are part of structs, and a struct can have both shared and variant fields

```
type Expr := struct
  ty : Ty           # shared fields
  source : Source
  case True         # cases with no fields
  case False
  case Identifier   # cases with fields
    value : String
  case IntLiteral
    value : int
end

let x := Expr.True{ty := ty; source := src}
let y := Expr.Identifier{ty := ty; source := src; value := name}
```

struct has a special `case` field whose type is a oneof & can be used in bitset

```
Expr.True{ ...} # a struct
Expr.True # a oneof value
Expr.case{True, False} # a set of case tags
```

## 'routines

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

---

Gremlin

I'm writing another programming language. The one I worked on last year was "Goblin", an homage to 90s scripting languages. This one is called "Gremlin", and its a low-level systems language for home computers of the late 1980s.

Here are the parameters for the project:

- static types
- semi-manual memory management
- single pass compiler
- some OOP-like features but an avoidance of "classes" or "objects"

To this end, it features a relatively simple static type system & manual memory management. The language can be compiled in a single pass, and individual modules can be compiled separately. There are parameterized types, but function parameters with generic types must be word-sized (ie primitives or pointers). OOP is largely avoided in favor of explicit dictionary-passing.

# Literals

```
123 # a 32-bit signed integer
1_234_567_890_123l # a 64-bit signed long integer
true # a boolean
"hello, world" # a string constant
```

# Assignment

```
let x := 123
let y : bool := false
x := 456
```

#
