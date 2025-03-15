Mutable variables can be defined using `mut` and referenced as usual. 

```zest-test
a mut = 1
a

1
```

To mutate a mutable variable, use `@`.

```zest-test
a mut = 1
a@ = a + 1
a

2
```

This works inside patterns too!

```zest-test
a mut = 1
b mut = 2
[a@, b@] = [b, a]
[a, b]

[2, 1]

undefined
```

```zest-test
a mut = 1
b mut = 2
[a@, c] = [b, a]
[a, b, c]

[2, 2, 1]

undefined
```

To pass a mutable reference to a function, mark the argument as `mut` and pass the reference with `@`.

```zest-test
a mut = 1
set = (var mut, val) { var@ = val }
set(a@, 2)
a

2
```

Every `mut` binding in the function must be paired with an `@` reference in the call site.

```zest-test
a mut = 1
set = (var mut, val) { var@ = val }
set(a, 2)
a

Expected a mutable reference, found: 1

Expected a mutable reference, found: i64
```

```zest-test
a mut = 1
set = (var, val) { var@ = val }
set(a@, 2)
a

May not mututate immutable binding: var
```

```zest-test
a mut = 1
set = (var, val) { }
set(a@, 2)
a

Expected a value containing no mutable references, found: 1/ref[i64]

Expected a value containing no mutable references, found: ref[i64]
```

If a mutable reference contains an object, mutable references may be constructed to fields of that object.

```zest-test
a mut = [b: 1]
a.b@ = 2
a

[b: 2]

undefined
```

```zest-test
a mut = [b: 1]
set = (var mut, val) { var@ = val }
set(a.b@, 2)
a

[b: 2]

undefined
```

TODO Mutable references never alias - setting the value of one mutable variable will never change the value of another mutable variable. To enforce this, all of the mutable arguments to a function call must be disjoint.

```zest-test
a mut = [b: 1, c: 2]
swap = (x mut, y mut) { 
  [x@, y@] = [y, x]
}
swap(a.b@, a.c@)
a

[b: 2, c: 1]

undefined
```

```zest-test
a mut = [b: 1, c: 2]
swap = (x mut, y mut) { 
  [x@, y@] = [y, x]
}
swap(a.b@, a.b@)
a

[b: 1, c: 2]

undefined
```

Only the mutable arguments are required to be disjoint - other arguments may overlap.

```zest-test
a mut = [b: 1, c: 2]
set = (var mut, val) { var@ = val }
set(a.b@, a.b)
a

[b: 1, c: 2]

undefined
```

Mutable references are 2nd-class - they cannot be assigned or returned.

```zest-test
a mut = 1
a@

Expected a value containing no mutable references, found: 1/ref[i64]

Expected a value containing no mutable references, found: ref[i64]
```

```zest-test
a mut = 1
b = a@

Expected a value containing no mutable references, found: 1/ref[i64]

Expected a value containing no mutable references, found: ref[i64]
```

```zest-test
a mut = 1
b = [a@]

Expected a value containing no mutable references, found: [1/ref[i64]]

Expected a value containing no mutable references, found: struct[ref[i64]]
```

Assigning to a mutable reference may not change the type of underlying variable:

```zest-test
a mut = 1
a@ = 'foo'

Expected i64, found string
```

```zest-test
a mut = [b: 1]
a.b@ = 'foo'

Expected i64, found string
```