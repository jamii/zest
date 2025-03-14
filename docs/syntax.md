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

```
a = 2
b = 1
a - b

1
```

```
a = 2
b = 1
a-b

Name not bound: a-b
```

```
a = 2
b = 1
a / b

TODO eval: dir.ExprData{ .call_builtin = zest.Builtin.divide }

TODO infer: dir.ExprData{ .call_builtin = zest.Builtin.divide }
```

```
a = 2
b = 1
a /b

Parse error: expected space or newline, found zest.TokenData.name
At 3:4:
a /b
    ^
```

```
a = 1
inc = (x) x + 1
a/inc()

2
```

TODO prefer this spacing over the one below

```
a = 1
inc = (x) x + 1
a /inc()

Parse error: expected space or newline, found zest.TokenData.name
At 3:6:
a /inc()
      ^
```

```
a = 1
inc = (x) x + 1
a/ inc()

2
```

## newlines

To make semicolon insertion safe, newlines are only allowed between items in `{}`, `()` and `[]` and after binary ops.

```
[a: 1,
 b: 2]

['a': 1, 'b': 2]

undefined
```

```
[
  a: 1,
  b: 2,
]

['a': 1, 'b': 2]

undefined
```

```
[
  a: 
    1,
  b:
    2,
]

Parse error: expected expr-atom, found zest.TokenData.newline
At 3:0:
    1,
^
```

```
inc = (a) a + 1
inc(1)

2
```

```
inc = (a) 
  a + 1
inc(1)

Parse error: expected expr-atom, found zest.TokenData.newline
At 2:0:
  a + 1
^
```

```
inc = (a) {
  a + 1
}
inc(1)

2
```

```
1 +
  1

2
```

```
1
+ 1

Parse error: expected expr-atom, found zest.TokenData.+
At 2:1:
+ 1
 ^
```

```
inc = (x) x + 1
1/inc()

2
```

TODO prefer this spacing over the one below

```
inc = (x) x + 1
1/
inc()

2
```

```
inc = (x) x + 1
1
  /inc()

Parse error: expected expr-atom, found zest.TokenData./
At 3:3:
  /inc()
   ^
```