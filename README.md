# gremlin_rs

A low level programming language

next up:

- forward context

refactors:

- ref unary op expr should be own ast node
- use constructor fns
- compile errors start with blank source_info & add when propagating up
- clean up handling of semicolons

# homermobile zone

## enums & tagged variants

oneof type declaration

```
type TrafficLight := oneof Red, Yellow, Green end
```

oneof expression (a bitset)

```
let should_go := oneof{Yellow,Green}
if contains(should_go, color) then ... end
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
oneof{Expr.True, Expr.False} # a set of case tags
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
