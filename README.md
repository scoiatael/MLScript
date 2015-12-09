# MLScript

Simple language with ml-like syntax and dynamic types.

Compiled to native code, with help of clang LLVM.

## Examples

```
let x = fun x -> 2*x

datatype Foo = Bar of . | Baz of (. .)

let y = fun z -> case z : Foo of
    | Bar x -> x + 2
    | Baz (x,y) -> x + y
```
