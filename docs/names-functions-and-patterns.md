## names

Names start with a lowercase letter and can contain lowercase letters, numbers and hyphens.

```zest-test
some-name = 'foo'
some-name

'foo'
```

```zest-test
side2side = 'foo'
side2side

'foo'
```

```zest-test
NoCaps = 'foo'
NoCaps

Tokenize error
At 1:0:
NoCaps = 'foo'
^
```

```zest-test
0digits-at-start = 'foo'
0digist-at-start

Parse error: expected eof, found sir.TokenData.name
At 1:16:
0digits-at-start = 'foo'
                ^
```

```zest-test
0-digits-at-start = 'foo'
0-digits-at-start

Parse error: expected eof, found sir.TokenData.-
At 1:2:
0-digits-at-start = 'foo'
  ^
```

In objects, if a key is a valid name then it is treated as a string.

```zest-test
foo = 'the number'
[foo: 42]

[foo: 42]

['foo': 42]
```

Use brackets to treat the key as a name.

```zest-test
foo = 'the number'
[{foo}: 42]

['the number': 42]

Cannot unstage value: string
```

If the key is a valid name then the value can be omitted and will be replaced by the value of that name.

```zest-test
foo = 'the number'
[:foo]

[foo: 'the number']

['foo': 'the number']
```

Names may not be shadowed.

```zest-test
foo = 1
foo = 2

Name already bound: foo
```

```zest-test
foo = 1
bar = () { 
  foo = 2 
}

Name already bound: foo
```

```zest-test
foo = 1
bar = (foo) 2

Name already bound: foo
```

## keys

The `.` operator retrieves the value associated with a key in an object.

```zest-test
abc = [a: 1, b: 2, c: 3]
abc.'b'

2
```

When the key is a string which is a valid name, the quotes can be omitted:

```zest-test
abc = [a: 1, b: 2, c: 3]
abc.b

2
```

Use brackets to treat the key as a name:

```zest-test
abc = [a: 1, b: 2, c: 3]
b = 'c'
abc.{b}

3

Cannot unstage value: string
```

If the key is not present in the map then an error is thrown.

```zest-test
abc = [a: 1, b: 2, c: 3]
abc.d

Key 'd' not found in [a: 1, b: 2, c: 3]

Key 'd' not found in struct[a: i64, b: i64, c: i64]
```

## functions

```zest-test
foo = (x) x + 1
foo(1)

2
```

Functions close over variables in their scope.

```zest-test
n = 1
inc = (x) x + n
inc(1)

2
```

Functions can be passed as arguments:

```zest-test
twice = (x, f) f(f(x))
twice(1, (x) x + 1)

3
```

Each function has a unique nominal type.

```zest-test
inc = (x) x + 1
%repr-of(inc)

fun[44]

TODO infer: dir.ExprData{ .repr_of = void }
```

```zest-test
inc = (x) x + 1
dec = (x) x - 1
%repr-of(inc) == %repr-of(dec)

0

TODO infer: dir.ExprData{ .repr_of = void }
```

The type of a function also depends on the type of the values it closes over:

```zest-test
make-echo = (x) () x
[
  %repr-of(make-echo(42)), 
  %repr-of(make-echo('foo')), 
]

[fun[46, x: i64], fun[46, x: string]]

TODO infer: dir.ExprData{ .repr_of = void }
```

Functions can be chained using the `/` operator.

```zest-test
inc = (x) x + 1
add = (x, y) x + y
1/inc()/add(2)

4
```

## patterns

Assignments can pattern-match values.

```zest-test
[x: x, y: y] = [x: 1, y: 2]
[x, y]

[1, 2]
```

Patterns that don't match exactly will throw errors.

```zest-test
[x: x, y: y] = [x: 1]
[x, y]

Expected 2 keys, found 1 keys
```

```zest-test
[x: x, y: y] = [x: 1, y: 2, z: 3]
[x, y]

Expected 2 keys, found 3 keys
```

Patterns can use the same syntax sugar as object fields:

```zest-test
[x, :y, 'z with spaces': z] = [0: 1, y: 2, 'z with spaces': 3]
[x, y, z]

[1, 2, 3]
```

The arguments to functions are just an object pattern-match:

```zest-test
foo = (x, :y, 'z with spaces': z) [x,y,z]
y = 2
foo(1, 'z with spaces': 3, :y)

[1, 2, 3]
```

TODO matching rest-of-object