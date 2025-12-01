Namespaces contain a sequence of variable bindings:

```zest-test
namespace{
  even = 4
  odd = 7
}

[]/namespace[1]

[]/namespace[TODO print]
```

Destructuring is not supported.

```zest-test
namespace{
  [even, odd] = [4, 7]
}

Namespace definitions must be names, not patterns
```

Mutable variables are not allowed.

```zest-test
namespace{
  even mut = 4
  odd mut = 7
}

Namespace definitions may not be mutable
```

Namespaces are first-class values.

```zest-test
kinds = namespace{
  even = 4
  odd = 7
}
[kinds, []/namespace[0]]

[[]/namespace[1], []/namespace[0]]

[[]/namespace[TODO print], []/namespace[TODO print]]
```

Bindings can be accessed with `..` (placeholder syntax.)

```zest-test
kinds = namespace{
  even = 4
  odd = 7
}
kinds..even

4
```

Bindings are evaluated lazily when interpreted, or at compile-time when compiled.

```zest-test
namespace{
  danger = %panic()
}

[]/namespace[1]

[]/namespace[TODO print]
```

```zest-test
n = namespace{
  danger = %panic()
}
n..danger

panic
```

Bindings are evaluated in a pure context.

```zest-test
n = namespace{
  impure = %print('hi!')
}
n..impure

Tried to perform a side effect during pure evaluation: zest.Builtin.print
```

Bindings can refer to each other in any order:

```zest-test
kinds = namespace{
  even = odd - 3
  odd = 7
}
kinds..even

4
```

Circular definitions will cause a panic when evaluated:

```zest-test
kinds = namespace{
  even = odd - 3
  odd = even + 3
}
kinds..even

Recursive evaluation: []/namespace[1]..'even'
```

Bindings may contain mutually recursive functions:

```zest-test
kinds = namespace{
  is-even = (n) /string {
    if {n == 0} {
      'true' 
    } else if {n == 1} {
      'false'
    } else is-even(n - 2)
  }
}
kinds..is-even(4)

'true'
```

But these functions must have return type annotations in order to type-check succesfully.

```zest-test
// TODO limit inference to annotations only
kinds = namespace{
  is-even = (n) {
    if {n == 0} {
      'true' 
    } else if {n == 1} {
      'false'
    } else is-even(n - 2)
  }
}
kinds..is-even(4)

'true'
```

```zest-test
// TODO limit inference to annotations only
kinds = namespace{
  is-even = (n) {
    if {n > 2} {
      is-even(n - 2)
    } else if {n == 1} {
      'false'
    } else {
      'true'
    }
  }
}
kinds..is-even(4)

'true'

Recursive inference: tir.FunKey{ .fun = dir.Fun{ .id = 41 }, .closure_repr = struct[], .arg_reprs = { struct[i64] } }
```

Namespaces currently can't close over values from the outer scope.

```zest-test
a = 1
n = namespace{
  b = a + 1
}
n..b

Name not bound: a
```
