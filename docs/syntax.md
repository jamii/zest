## hunches

`[]` always indicates constructing a value. The result prints like the syntax.

`()` always indicates computation: either defining (`(x) x + 1`) or performing (`inc(x)`).

`{}` is used for grouping instead of `()` to avoid ambiguity.

`foo.bar` is used for key lookups to roughly mirror the `:` in key definition `foo = [bar: 42]`.

The syntax for function patterns is consistent with the syntax for creating structs - as if functions take a struct of arguments.

Parsing requires only 2-token lookahead (TODO verify once all tests pass).

I gave up on trying to avoid the shift key - there aren't enough good shiftless symbols to indicate all the major groups of syntax.

## spaces

Whitespace between tokens is often significant.

```zest-test
a = 2
b = 1
a - b

1
```

```zest-test
a = 2
b = 1
a-b

Name not bound: a-b
```

```zest-test
a = 2
b = 1
a / b

TODO eval: dir.ExprData{ .call_builtin = zest.Builtin.divide }

TODO infer: dir.ExprData{ .call_builtin = zest.Builtin.divide }
```

```zest-test
a = 2
b = 1
a /b

Parse error: expected space or newline, found sir.TokenData.name
At 3:4:
a /b
    ^
```

```zest-test
a = 1
inc = (x) x + 1
a/inc()

2
```

TODO prefer this spacing over the one below

```zest-test
a = 1
inc = (x) x + 1
a /inc()

Parse error: expected space or newline, found sir.TokenData.name
At 3:6:
a /inc()
      ^
```

```zest-test
a = 1
inc = (x) x + 1
a/ inc()

2
```

## newlines

To make semicolon insertion safe, newlines are only allowed between items in `{}`, `()` and `[]` and after binary ops.

```zest-test
[a: 1,
 b: 2]

[a: 1, b: 2]

undefined
```

```zest-test
[
  a: 1,
  b: 2,
]

[a: 1, b: 2]

undefined
```

```zest-test
[
  a: 
    1,
  b:
    2,
]

Parse error: expected expr-atom, found sir.TokenData.newline
At 3:0:
    1,
^
```

```zest-test
inc = (a) a + 1
inc(1)

2
```

```zest-test
inc = (a) 
  a + 1
inc(1)

Parse error: expected expr-atom, found sir.TokenData.newline
At 2:0:
  a + 1
^
```

```zest-test
inc = (a) {
  a + 1
}
inc(1)

2
```

```zest-test
1 +
  1

2
```

```zest-test
1
+ 1

Parse error: expected expr-atom, found sir.TokenData.+
At 2:1:
+ 1
 ^
```

```zest-test
inc = (x) x + 1
1/inc()

2
```

TODO prefer this spacing over the one below

```zest-test
inc = (x) x + 1
1/
inc()

2
```

```zest-test
inc = (x) x + 1
1
  /inc()

Parse error: expected expr-atom, found sir.TokenData./
At 3:3:
  /inc()
   ^
```