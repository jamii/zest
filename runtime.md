Runtime implements:
* Allocator
* Copying/refcounting
* Lists
* Hashmaps
* Strings

Going to need integers:

```
42

42
```

```
{42 + 1} - 3

40
```

Structs:

```
[1, /a 2]/0

1
```

```
[1, /a 2]/a

2
```

```
[1, /a 2]/b

Key 'b' does not exist in struct[i64, /a i64]
```

TODO Maybe unions? Or enums?

Variables:

```
a = 1
b = 2
a + b

3
```

```
a = 1
{
  b = 2
}
a + b

Name b not in scope
```

Comparisons:

```
1 < 2

1
```

```
1 > 2

0
```

```
1 <= 2

1
```

```
1 >= 2

0
```

```
1 == 2

0
```

Mutation:

```
a = @42
@a = a + 1
a

43
```

```
a = @[42]
@a = 43
a

Expected struct[i64], found i64
```

But be careful about aliasing:

```
x = @[/a 1, /b 2]
@x = [/a x/b, /b x/a]
x/a + x/b

error
```

```
x = @[/a 1, /b 2]
y = [/a x/b, /b x/a]
@x = y
x/a + x/b

3
```

Functions (but not higher-order):

```
inc = (@a) @a = a + 1
x = @42
inc(@x)
x

43
```

Control flow:

```
if {1} 3 5

3
```

```
if {0} 3 5

5
```

```
a = @10
b = @0
while {a > 0} {
  @a = a - 1
  @b = b + 1
}
b

10
```

Panics:

```
panic()

panic
```

Operations on reprs:

```
a = 1
b = 0
repr-of(a / b) // doesn't evaluate argument

i64
```

```
struct[i64, /b i64].size-of()

16
```

```
struct[i64, /b i64].align-of()

3
```

TODO Operations on memory:
* store/load/copy
  * take constant repr argument
* size/shrink/grow
* stack-size()
  * turned into constant at compile-time

TODO Modules?