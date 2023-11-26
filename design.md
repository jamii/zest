## data

Encoded as utf8.

### numbers

Arbitrary precision decimals.

```
42

42
```

```
3.14

3.14
```

TODO NaN? Inf?

TODO other number notations.

### strings

A sequence of unicode characters.

```
'foo'

'foo'
```

TODO standard escapes

```
'have a \'string\''

'have a \'string\''
```

No literal newlines.

```
'not a
string'

syntax-error
```

### objects

Zero or more key-value pairs, separated by commas.

```
[]

[]
```

```
['a' = 'apple', 'b' = 'bear']

['a' = 'apple', 'b' = 'bear']
```

Optional trailing comma.

```
['a' = 'apple', 'b' = 'bear',]

['a' = 'apple', 'b' = 'bear']
```

Key order doesn't matter. Keys may be printed in any order.

```
['b' = 'bear', 'a' = 'apple']

['a' = 'apple', 'b' = 'bear']
```

No repeated keys.

```
['a' = 'apple', 'a' = 'bear']

syntax-error
```

Omitted keys default to consecutive integers, starting at 0.

```
['a', 'b']

['a', 'b']
```

```
[0 = 'a', 1 = 'b']

['a', 'b']
```

Can mix omitted and present keys.

```
['a', 1 = 'b', 'default' = 'c']

['a', 'b', 'default' = 'c']
```

But omitted keys must be written before present keys.

```
['a', 'default' = 'c', 'b']

syntax-error
```

## repr

A value constists of a datum and a repr(esentation). 

A repr:
* Maps some subset of datums to bytes in memory.
* Constrains the available builtin operations on those datums.

Every datum has a default repr:
* Numbers default to big-int, or big-dec if they have digits after the decimal point.
* Strings default to string.
* Objects default to structs.

When printing values, the repr can be omitted if it is the default repr for the datum, or if it determined by a surrounding repr.

### integers

```
i64[42]

i64[42]
```

```
i64[9,223,372,036,854,775,808]

error
```

```
i64[3.14]

error
```

### floats

```
f64[42]

f64[42]
```

```
f64[9,223,372,036,854,775,808]

error
```

```
f64[3.14]

f64[3.14]
```

### strings

```
string['foo']

'foo'
```

Strings are uft8.

Strings allow pushing and popping unicode characters.

(We don't need to distinguish between strings and string slices because we copy-on-write anyway).

### structs

Structs are objects with a fixed, finite set of keys.

```
struct['a' = i64, 'b' = i64]['a' = 0, 'b' = 1]

['a' = 0, 'b' = 1]
```

```
struct['a' = f64, 'b' = i64]['a' = 0, 'b' = 1]

['a' = f64[0], 'b' = 1]
```

```
struct['a' = f64, 'b' = i64]['a' = 0]

error
```

```
struct['a' = f64, 'b' = i64]['a' = 0, 'b' = 1, 'c' = 2]

error
```

The keys are not required to be strings!

```
struct[f64, i64][0, 1]

[f64[0], 1]
```

```
struct[['a', 'b'] = string][['a', 'b'] = 'c']

[['a', 'b'] = 'c']
```

Structs are laid out contiguously in memory, in some implementation-defined order.

TODO May want to define key order for binary serialization?

Structs allow getting and setting keys, but not deleting or adding keys.

### lists

Lists are objects where the keys are consecutive integers beginning with 0 and the values all have the same repr.

```
list[f64][]

list[f64][]
```

```
list[f64][0, 1, 2]

list[f64][0, 1, 2]
```

```
list[string][0, 1, 2]

error
```

```
list[f64]['a' = 'apple']

error
```

```
list[f64][1 = 3.14]

error
```

Lists are laid out contiguously in memory, in key order.

Lists allow getting and setting keys, and pushing/popping.

(We don't need to distinguish between strings and string slices because we copy-on-write anyway).

### maps

Maps are objects with any number of entries, where all the keys have the same repr and all the values have the same repr. 

```
map[i64, string][0 = 'zero', 1 = 'one']

map[i64, string][0 = 'zero', 1 = 'one']
```

```
map[i64, string][0 = 'zero', 1 = 1]

error
```

Maps are hashtables. Keys and values are stored inline.

Maps support getting/setting/adding/deleting keys.

TODO Make a decision about iteration order. Options:
* Implementation-defined. Breaks determinism.
* Sorted order. Expensive.
* Insertion order (like js maps). Then have to say that either key order matters in notation, or that equal maps can have different iteration orders.

### union

A union contains one of a fixed set of reprs.

```
union[string, i64][i64[42]]

union[string, i64][i64[42]]
```

```
union[string, i64]["foo"]

union[string, i64]["foo"]
```

```
union[string, i64][3.14]

error
```

Unions are represented by an integer tag denoting the repr, followed by the representation of their value.

Unions support asking for the repr of their value and casting the value to a given repr.

### any

An any can represent any datum.

```
any[i64[42]]

any[i64[42]]
```

```
any["foo"]

any["foo"]
```

Anys are represented by a pointer to a value. 

Anys support asking for the repr of their value and casting the value to a given repr.

### only

An only is an object with exactly one entry. Both the key and the value are stored in the repr itself.

```
only[i64[42]][]

only[i64[42]][]
```

Onlys are zero-sized.

Their main purpose is lifting values into reprs, where they are visible to specialization, type inference and compile-time computations.

```
only[i64[42]][]/get-repr

only[i64[42]]
```

```
only[i64[42]]/get-value

i64[42]
```

### repr repr

Reprs are themselves datums. Their default repr is `repr`.

```
i64/get-repr

repr
```

We are saved from infinite recursion by:

```
repr/get-repr

only[repr]
```

The in-memory layout of repr is not exposed.

## equality

Two __datums__ are equal if:
* They are both numbers, and they are equal (TODO scary can of worms).
* They are both strings, and they contain the same sequence of unicode characters.
* They are both objects, and:
  * They contain the same set of keys.
  * For each key, they contain the same value.

Two __values__ are `==` if they have the same repr and their datums are equal.

```
i64[42] == i64[42]

true
```

```
i64[42] == i64[1]

false
```

```
i64[42] == f64[42]

false
```

```
["a" = 1, "b" = 2] == ["b" = 2, "a" = 1]

true
```

```
["a" = 1, "b" = 2] == ["b" = 100, "a" = 1]

false
```

```
["a" = 1, "b" = 2] == ["b" = 2, "a" = 1, "c" = 3]

false
```

```
["a" = 1, "b" = 2] == map[string, i64]["b" = 2, "a" = 1]

false
```

Two __values__ are `~=` if their datums are equal.

```
i64[42] != i64[42]

true
```

```
i64[42] != i64[1]

false
```

```
i64[42] != f64[42]

true
```

```
["a" = 1, "b" = 2] != ["b" = 2, "a" = 1]

true
```

```
["a" = 1, "b" = 2] != ["b" = 100, "a" = 1]

false
```

```
["a" = 1, "b" = 2] != ["b" = 2, "a" = 1, "c" = 3]

false
```

```
["a" = 1, "b" = 2] != map[string, i64]["b" = 2, "a" = 1]

true
```

TODO What about NaN? 

TODO What about +0 vs -0.

## ordering

TODO < for type then datum, ~< for datum only

## as

The function `as` creates a new value with the same datum but a different repr.

```
i64[42]/as[f64]

f64[42]
```

If the new repr cannot encode the datum, `as` throws an error.

```
f64[3.14]/as[i64]

error
```

If the combination of reprs is such that `as` can never throw an error then the return type will not include an error.

```
as-int = fn [x] x/as[i64]
[return-type[as-int, [i64]], return-type[as-int, [f64]]

[i64, union[i64, error]]
```

## errors

TODO so many decisions

## misc

TODO We use value for both repr+datum and key-value. Think of a better name for the former.

TODO Using [] for both objects and function calls is probably bad for readability.

TODO Add examples of supported operations.