A value consists of:
* Some data
* A type

Data can be (arbitrary-precision decimal) numbers, (utf8) strings, or objects.

The type controls:
* How the data is represented in memory.
* What operations are supported on that data.

The intention is that types which encode the same data should generally behave the same way. For example, the `get` function should behave the same way whether it's first argument is an object with a `struct[foo: i64]` type or an object with a `map[string, i64]` type.

The language only has syntax for a few basic types:
* Integer numbers default to `i64`.
* Non-integer numbers default to `f64`.
* Strings default to `string`.
* Objects default to `struct`, with the keys and value types determined recursively.

```
%repr-of(42)

i64

TODO infer: dir.ExprData{ .repr_of = void }
```

```
%repr-of(3.14)

TODO desugar: sir.ExprData{ .f64 = 3.14e0 }
```

```
%repr-of('foo')

string

TODO infer: dir.ExprData{ .repr_of = void }
```

```
%repr-of([a: 42, b: 'foo'])

struct['a': i64, 'b': string]

TODO infer: dir.ExprData{ .repr_of = void }
```

All other types can be built via conversions from these basic types:

```
f64[42]

Name not bound: f64
```

```
union[a: i64, b: string][[a: 42]]

union['a': i64, 'b': string][['a': 42]]

undefined
```

```
map[string, i64][[a: 42]]

Name not bound: map
```

Types are themselves values.

```
union[a: i64, b: string]

union['a': i64, 'b': string]

TODO infer: dir.ExprData{ .repr_i64 = void }
```

```
%repr-of(union[a: i64, b: string])

repr

TODO infer: dir.ExprData{ .repr_of = void }
```

```
t = union[a: i64, b: string]
t[[a: 42]]

union['a': i64, 'b': string][['a': 42]]

TODO infer: dir.ExprData{ .repr_i64 = void }
```

## data

### numbers

Arbitrary precision decimals.

```
42

42
```

```
3.14

TODO desugar: sir.ExprData{ .f64 = 3.14e0 }
```

TODO NaN? Inf?

TODO other number notations.

### strings

A sequence of unicode characters enclosed in single quotes.

```
'foo'

'foo'

undefined
```

The usual escapes are supported.

TODO print escapes properly.

```
'have a \'string\''

'have a 'string''

undefined
```

```
'escape \\ this'

'escape \ this'

undefined
```

The syntax does not allow literal newlines.

```
'not a
string'

Tokenize error
At 1:0:
'not a
^
```

```
'a\nstring'

'a
string'

undefined
```

### objects

Zero or more key-value pairs, separated by commas.

```
[]

[]

undefined
```

```
['a': 'apple', 'b': 'bear']

['a': 'apple', 'b': 'bear']

undefined
```

Optional trailing comma.

```
['a': 'apple', 'b': 'bear',]

['a': 'apple', 'b': 'bear']

undefined
```

TODO Key order doesn't matter. Keys may be printed in any order.

```
['b': 'bear', 'a': 'apple']

['b': 'bear', 'a': 'apple']

undefined
```

TODO No repeated keys.

```
['a': 'apple', 'a': 'bear']

['a': 'apple', 'a': 'bear']

undefined
```

If the key is a string which is a valid name, the quotes may be omitted.

```
[a: 'apple', b: 'bear']

['a': 'apple', 'b': 'bear']

undefined
```

Omitted keys default to consecutive integers, starting at 0.

```
['a', 'b']

['a', 'b']

undefined
```

```
[0: 'a', 1: 'b']

['a', 'b']

undefined
```

```
[1: 'b', 0: 'a',]

[1: 'b', 0: 'a']

undefined
```

You can mix omitted and present keys.

```
['a', 1: 'b', default: 'c']

['a', 'b', 'default': 'c']

undefined
```

But omitted keys must be written before present keys.

```
['a', default: 'c', 'b']

Parse error: all positional args must appear before all keyed args
At 1:23:
['a', default: 'c', 'b']
                       ^
```

## types

### booleans

TODO

### integers

```
i64[42]

42
```

```
i64[9223372036854775808]

Parse error: invalid i64: parse.ParseErrorData__enum_24720.overflow
At 1:23:
i64[9223372036854775808]
                       ^
```

```
i64[3.00]

TODO desugar: sir.ExprData{ .f64 = 3e0 }
```

```
i64[3.14]

TODO desugar: sir.ExprData{ .f64 = 3.14e0 }
```

### floats

```
f64[42]

Name not bound: f64
```

TODO Represent literals as big-intb:ig-dec to avoid this problem.

```
f64[9223372036854775808]

Parse error: invalid i64: parse.ParseErrorData__enum_24720.overflow
At 1:23:
f64[9223372036854775808]
                       ^
```

TODO Don't allow imprecise float parse.

```
f64[9223372036854775808.0]

Name not bound: f64
```

```
f64[3.14]

Name not bound: f64
```

### strings

```
string['foo']

'foo'

undefined
```

Strings are uft8.

Strings allow pushing and popping unicode characters.

### structs

Structs are objects with a fixed set of keys.

```
struct[a: i64, b: string][[a: 0, b: 'foo']]

['a': 0, 'b': 'foo']

undefined
```

```
struct[a: i64, b: i64][[a: 42, b: 'foo']]

Expected struct['a': i64, 'b': i64], found struct['a': i64, 'b': string]
```

```
struct[a: i64, b: string][[a: 42]]

Expected struct['a': i64, 'b': string], found struct['a': i64]
```

```
struct[a: i64, b: string][[a: 42, b: 'foo', c: 99]]

Expected struct['a': i64, 'b': string], found struct['a': i64, 'b': string, 'c': i64]
```

The keys are not required to be strings!

```
%repr-of([42, 'foo'])

struct[i64, string]

TODO infer: dir.ExprData{ .repr_of = void }
```

```
%repr-of([['a', 'b']: 'c'])

struct[['a', 'b']: string]

TODO infer: dir.ExprData{ .repr_of = void }
```

Structs are laid out contiguously in memory, in some implementation-defined order.

TODO May want to define key order for binary serialization?

Structs allow getting and setting keys, but not deleting or adding keys.

### unions

A union represents one of a finite number of single-key objects.

```
union[strings: string, nums: i64][[strings: 'hello']]

union['strings': string, 'nums': i64][['strings': 'hello']]

undefined
```

```
union[strings: string, nums: i64][[nums: 'hello']]

Expected union['strings': string, 'nums': i64], found struct['nums': string]
```

```
union[strings: string, nums: i64][[floats: 3.14]]

TODO desugar: sir.ExprData{ .f64 = 3.14e0 }
```

```
x = union[strings: string, nums: i64][[strings: 'hello']]
x.strings

'hello'

undefined
```

```
x = union[strings: string, nums: i64][[strings: 'hello']]
x.nums

Key 'nums' not found in union['strings': string, 'nums': i64][['strings': 'hello']]

RuntimeError: unreachable
    at <anonymous> (wasm://wasm/3efd2dd2:1:168)
    at file:///home/jamie/zest/test.js:33:39
```

```
x = union[strings: string, nums: i64][[strings: 'hello']]
x/has('strings')

Name not bound: has
```

Unions are represented by an integer tag (eg 0 for 'strings', 1 for 'nums') followed by the value.

### lists

Lists are objects where the keys are consecutive integers beginning with 0 and the values all have the same type.

```
list[f64][[]]

Name not bound: list
```

```
list[f64][[0, 1, 2]]

Name not bound: list
```

```
list[string][[0, 1, 2]]

Name not bound: list
```

```
list[f64][[a: 'apple']]

Name not bound: list
```

```
list[f64][[1: 3.14]]

Name not bound: list
```

Lists are laid out contiguously in memory, in key order.

Lists allow getting and setting keys, and pushing/popping.

### maps

Maps are objects with any number of entries, where all the keys have the same type and all the values have the same type.

```
map[string, i64][[zero: 0, one: 1]]

Name not bound: map
```

```
map[string, i64][[zero: 0, one: 'one']]

Name not bound: map
```

Maps are hashtables. Keys and values are stored inline.

Maps support getting/setting/adding/deleting keys.

TODO Make a decision about iteration order (see notes [here](https://www.scattered-thoughts.net/log/0048/#zest-ordering)). Options:
* Implementation-defined. Breaks determinism.
* Sorted order. Expensive.
* Insertion order (like js maps). Then have to say that either key order matters in notation, or that equal maps can have different iteration orders.

## equality

Two __data__ are equal if:
* They are both numbers, and they represent equal numbers (TODO what about NaN, +0 vs -0, floating point error).
* They are both strings, and they contain the same sequence of unicode characters.
* They are both objects, and:
  * They contain equal sets of keys.
  * For each key, they contain the equal associated values.
* They are both atomic types (i64, f64, string, any), and they are the same type.
* They are both compound types (struct, union, list, map), and:
  * They both have the same type constructor.
  * The arguments to their constructors are equal.

Two __values__ are `~=` if their data are equal.

Two __values__ are `==` if they their types are equal and their data are equal. Calling `==` with values of different types causes a type error.

It's intended that `a ~= b` iff `a == b/convert(type-of(a))` ie convert should only ever change the type and not the data.

```
42 == 42

1
```

```
42 == 99

0
```

```
42 == 42.0

TODO desugar: sir.ExprData{ .f64 = 4.2e1 }
```

```
42 ~= 42.0

TODO desugar: sir.ExprData{ .f64 = 4.2e1 }
```

```
[a: 1, b: 2] == [b: 2, a: 1]

Cannot call zest.Builtin.equal with these args: { ['a': 1, 'b': 2], ['b': 2, 'a': 1] }

Cannot call zest.Builtin.equal with these args: { struct['a': i64, 'b': i64], struct['b': i64, 'a': i64] }
```

```
[a: 1, b: 2] == [b: 100, a: 1]

Cannot call zest.Builtin.equal with these args: { ['a': 1, 'b': 2], ['b': 100, 'a': 1] }

Cannot call zest.Builtin.equal with these args: { struct['a': i64, 'b': i64], struct['b': i64, 'a': i64] }
```

```
[a: 1, b: 2] == [b: 2, a: 1, c: 3]

Cannot call zest.Builtin.equal with these args: { ['a': 1, 'b': 2], ['b': 2, 'a': 1, 'c': 3] }

Cannot call zest.Builtin.equal with these args: { struct['a': i64, 'b': i64], struct['b': i64, 'a': i64, 'c': i64] }
```

```
[a: 1, b: 2] == map[string, i64][[b: 2, a: 1]]

Name not bound: map
```

```
[a: 1, b: 2] != map[string, i64][[b: 2, a: 1]]

Name not bound: map
```

```
union[strings: string, nums: i64][[nums: 42]] ~= struct[nums: i64][[nums: 42]]

TODO eval: dir.ExprData{ .call_builtin = zest.Builtin.equivalent }

TODO infer: dir.ExprData{ .call_builtin = zest.Builtin.equivalent }
```

```
struct[name: string, age: i64] == struct[name: string, age: i64]

Cannot call zest.Builtin.equal with these args: { struct['name': string, 'age': i64], struct['name': string, 'age': i64] }

TODO infer: dir.ExprData{ .repr_string = void }
```

## ordering

TODO < to compare by type then data, ~< to compare by data only.