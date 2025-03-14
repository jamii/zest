## names

Names start with a lowercase letter and can contain lowercase letters, numbers and hyphens.

```
some-name = 'foo'
some-name

'foo'

undefined
```

```
side2side = 'foo'
side2side

'foo'

undefined
```

```
NoCaps = 'foo'
NoCaps

Tokenize error
At 1:0:
NoCaps = 'foo'
^
```

```
0digits-at-start = 'foo'
0digist-at-start

Parse error: expected eof, found zest.TokenData.name
At 1:16:
0digits-at-start = 'foo'
                ^
```

```
0-digits-at-start = 'foo'
0-digits-at-start

Parse error: expected eof, found zest.TokenData.-
At 1:2:
0-digits-at-start = 'foo'
  ^
```

In objects, if a key is a valid name then it is treated as a string.

```
foo = 'the number'
[foo: 42]

['foo': 42]

undefined
```

Use brackets to treat the key as a name.

```
foo = 'the number'
[{foo}: 42]

['the number': 42]

Cannot unstage value: string
```

If the key is a valid name then the value can be omitted and will be replaced by the value of that name.

```
foo = 'the number'
[:foo]

['foo': 'the number']

undefined
```

Names may not be shadowed.

```
foo = 1
foo = 2

Name already bound: foo
```

```
foo = 1
bar = () { 
  foo = 2 
}

Name already bound: foo
```

```
foo = 1
bar = (foo) 2

Name already bound: foo
```

## keys

The `.` operator retrieves the value associated with a key in an object.

```
abc = [a: 1, b: 2, c: 3]
abc.'b'

2
```

When the key is a string which is a valid name, the quotes can be omitted:

```
abc = [a: 1, b: 2, c: 3]
abc.b

2
```

Use brackets to treat the key as a name:

```
abc = [a: 1, b: 2, c: 3]
b = 'c'
abc.{b}

3

Cannot unstage value: string
```

If the key is not present in the map then an error is thrown.

```
abc = [a: 1, b: 2, c: 3]
abc.d

Key 'd' not found in ['a': 1, 'b': 2, 'c': 3]

Key 'd' not found in struct['a': i64, 'b': i64, 'c': i64]
```

## functions

```
foo = (x) x + 1
foo(1)

2
```

Functions close over variables in their scope.

```
n = 1
inc = (x) x + n
inc(1)

2
```

Functions can be passed as arguments:

```
twice = (x, f) f(f(x))
twice(1, (x) x + 1)

3
```

Each function has a unique nominal type.

```
inc = (x) x + 1
%repr-of(inc)

fun[id: 1, closure: struct[]]

TODO infer: dir.ExprData{ .repr_of = void }
```

```
inc = (x) x + 1
dec = (x) x - 1
%repr-of(inc) == %repr-of(dec)

Cannot call zest.Builtin.equal with these args: { fun[id: 1, closure: struct[]], fun[id: 3, closure: struct[]] }

TODO infer: dir.ExprData{ .repr_of = void }
```

The type of a function also depends on the type of the values it closes over:

```
make-echo = (x) () x
[
  %repr-of(make-echo(42)), 
  %repr-of(make-echo('foo')), 
]

[fun[id: 3, closure: struct['x': i64]], fun[id: 3, closure: struct['x': string]]]

TODO infer: dir.ExprData{ .repr_of = void }
```

## patterns

Assignments can pattern-match values.

```
[x: x, y: y] = [x: 1, y: 2]
[x, y]

[1, 2]

undefined
```

Patterns that don't match exactly will throw errors.

```
[x: x, y: y] = [x: 1]
[x, y]

Expected 2 keys, found 1 keys
```

```
[x: x, y: y] = [x: 1, y: 2, z: 3]
[x, y]

Expected 2 keys, found 3 keys
```

Patterns can use the same syntax sugar as object fields:

```
[x, :y, 'z with spaces': z] = [0: 1, y: 2, 'z with spaces': 3]
[x, y, z]

[1, 2, 3]

undefined
```

The arguments to functions are just an object pattern-match:

```
foo = (x, :y, 'z with spaces': z) [x,y,z]
y = 2
foo(1, 'z with spaces': 3, :y)

[1, 2, 3]

undefined
```

TODO matching rest-of-object