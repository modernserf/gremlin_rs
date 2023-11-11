# gremlin_rs

A low level programming language

next up:

- forward context
- long ints, size agreement in casts
- structs
- compiler moves take size param

refactors:

- ref unary op expr should be own ast node
- use constructor fns
- compile errors start with blank source_info & add when propagating up
- ok_or_else
- clean up handling of semicolons
