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

```zest-test
%repr-of(42)

i64

TODO infer: dir.ExprData{ .repr_of = void }
```

```zest-test
%repr-of(3.14)

TODO desugar: sir.ExprData{ .f64 = 3.14e0 }
```

```zest-test
%repr-of('foo')

string

TODO infer: dir.ExprData{ .repr_of = void }
```

```zest-test
%repr-of([a: 42, b: 'foo'])

struct['a': i64, 'b': string]

TODO infer: dir.ExprData{ .repr_of = void }
```

All other types can be built via conversions from these basic types:

```zest-test
42/f64

Name not bound: f64
```

```zest-test
[a: 42]/union[a: i64, b: string]

['a': 42]/union['a': i64, 'b': string]

undefined
```

```zest-test
[a: 42]/map[string, i64]

Name not bound: map
```

Types are themselves values.

```zest-test
union[a: i64, b: string]

union['a': i64, 'b': string]

TODO infer: dir.ExprData{ .repr_i64 = void }
```

```zest-test
%repr-of(union[a: i64, b: string])

repr

TODO infer: dir.ExprData{ .repr_of = void }
```

```zest-test
t = union[a: i64, b: string]
[a: 42]/t

['a': 42]/union['a': i64, 'b': string]

TODO infer: dir.ExprData{ .repr_i64 = void }
```

## data

### numbers

Arbitrary precision decimals.

```zest-test
42

42
```

```zest-test
3.14

TODO desugar: sir.ExprData{ .f64 = 3.14e0 }
```

TODO NaN? Inf?

TODO other number notations.

### strings

A sequence of unicode characters enclosed in single quotes.

```zest-test
'foo'

'foo'

undefined
```

The usual escapes are supported.

```zest-test
'have a \'string\''

'have a \'string\''

undefined
```

```zest-test
'escape \\ this'

'escape \\ this'

undefined
```

The syntax does not allow literal newlines.

```zest-test
'not a
string'

Tokenize error
At 1:0:
'not a
^
```

```zest-test
'a\nstring'

'a\nstring'

undefined
```

### objects

Zero or more key-value pairs, separated by commas.

```zest-test
[]

[]

undefined
```

```zest-test
['a': 'apple', 'b': 'bear']

['a': 'apple', 'b': 'bear']

undefined
```

Optional trailing comma.

```zest-test
['a': 'apple', 'b': 'bear',]

['a': 'apple', 'b': 'bear']

undefined
```

TODO Key order doesn't matter. Keys may be printed in any order.

```zest-test
['b': 'bear', 'a': 'apple']

['b': 'bear', 'a': 'apple']

undefined
```

TODO No repeated keys.

```zest-test
['a': 'apple', 'a': 'bear']

['a': 'apple', 'a': 'bear']

undefined
```

If the key is a string which is a valid name, the quotes may be omitted.

```zest-test
[a: 'apple', b: 'bear']

['a': 'apple', 'b': 'bear']

undefined
```

Omitted keys default to consecutive integers, starting at 0.

```zest-test
['a', 'b']

['a', 'b']

undefined
```

```zest-test
[0: 'a', 1: 'b']

['a', 'b']

undefined
```

```zest-test
[1: 'b', 0: 'a',]

[1: 'b', 0: 'a']

undefined
```

You can mix omitted and present keys.

```zest-test
['a', 1: 'b', default: 'c']

['a', 'b', 'default': 'c']

undefined
```

But omitted keys must be written before present keys.

```zest-test
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

```zest-test
i64[42]

42
```

```zest-test
i64[9223372036854775808]

Parse error: invalid i64: parse.ParseErrorData__enum_24740.overflow
At 1:23:
i64[9223372036854775808]
                       ^
```

```zest-test
i64[3.00]

TODO desugar: sir.ExprData{ .f64 = 3e0 }
```

```zest-test
i64[3.14]

TODO desugar: sir.ExprData{ .f64 = 3.14e0 }
```

### floats

```zest-test
f64[42]

Name not bound: f64
```

TODO Represent literals as big-intb:ig-dec to avoid this problem.

```zest-test
f64[9223372036854775808]

Parse error: invalid i64: parse.ParseErrorData__enum_24740.overflow
At 1:23:
f64[9223372036854775808]
                       ^
```

TODO Don't allow imprecise float parse.

```zest-test
f64[9223372036854775808.0]

Name not bound: f64
```

```zest-test
f64[3.14]

Name not bound: f64
```

### strings

```zest-test
string['foo']

'foo'

undefined
```

Strings are uft8.

Strings allow pushing and popping unicode characters.

### structs

Structs are objects with a fixed set of keys.

```zest-test
[a: 0, b: 'foo']/struct[a: i64, b: string]

['a': 0, 'b': 'foo']

undefined
```

```zest-test
[a: 42, b: 'foo']/struct[a: i64, b: i64]

Expected struct['a': i64, 'b': i64], found struct['a': i64, 'b': string]
```

```zest-test
[a: 42]/struct[a: i64, b: string]

Expected struct['a': i64, 'b': string], found struct['a': i64]
```

```zest-test
[a: 42, b: 'foo', c: 99]/struct[a: i64, b: string]

Expected struct['a': i64, 'b': string], found struct['a': i64, 'b': string, 'c': i64]
```

The keys are not required to be strings!

```zest-test
%repr-of([42, 'foo'])

struct[i64, string]

TODO infer: dir.ExprData{ .repr_of = void }
```

```zest-test
%repr-of([['a', 'b']: 'c'])

struct[['a', 'b']: string]

TODO infer: dir.ExprData{ .repr_of = void }
```

Structs are laid out contiguously in memory, in some implementation-defined order.

TODO May want to define key order for binary serialization?

Structs allow getting and setting keys, but not deleting or adding keys.

### unions

A union represents one of a finite number of single-key objects.

```zest-test
[strings: 'hello']/union[strings: string, nums: i64]

['strings': 'hello']/union['strings': string, 'nums': i64]

undefined
```

```zest-test
[nums: 'hello']/union[strings: string, nums: i64]

Expected union['strings': string, 'nums': i64], found struct['nums': string]
```

```zest-test
[floats: 3.14]/union[strings: string, nums: i64]

TODO desugar: sir.ExprData{ .f64 = 3.14e0 }
```

```zest-test
x = [strings: 'hello']/union[strings: string, nums: i64]
x.strings

'hello'

undefined
```

```zest-test
x = [strings: 'hello']/union[strings: string, nums: i64]
x.nums

Key 'nums' not found in ['strings': 'hello']/union['strings': string, 'nums': i64]

RuntimeError: unreachable
    at <anonymous> (wasm://wasm/3efd2dd2:1:168)
    at file:///home/jamie/zest/test.js:33:39
```

```zest-test
x = [strings: 'hello']/union[strings: string, nums: i64]
x/has('strings')

Name not bound: has
```

Unions are represented by an integer tag (eg 0 for 'strings', 1 for 'nums') followed by the value.

### lists

Lists are objects where the keys are consecutive integers beginning with 0 and the values all have the same type.

```zest-test
[]/list[f64]

Name not bound: list
```

```zest-test
[0, 1, 2]/list[f64]

Name not bound: list
```

```zest-test
[0, 1, 2]/list[string]

Name not bound: list
```

```zest-test
[a: 'apple']/list[f64]

Name not bound: list
```

```zest-test
[1: 3.14]/list[f64]

Name not bound: list
```

Lists are laid out contiguously in memory, in key order.

Lists allow getting and setting keys, and pushing/popping.

### maps

Maps are objects with any number of entries, where all the keys have the same type and all the values have the same type.

```zest-test
[zero: 0, one: 1]/map[string, i64]

Name not bound: map
```

```zest-test
[zero: 0, one: 'one']/map[string, i64]

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

```zest-test
42 == 42

1
```

```zest-test
42 == 99

0
```

```zest-test
42 == 42.0

TODO desugar: sir.ExprData{ .f64 = 4.2e1 }
```

```zest-test
42 ~= 42.0

TODO desugar: sir.ExprData{ .f64 = 4.2e1 }
```

```zest-test
[a: 1, b: 2] == [b: 2, a: 1]

Cannot call zest.Builtin.equal with these args: { ['a': 1, 'b': 2], ['b': 2, 'a': 1] }

Cannot call zest.Builtin.equal with these args: { struct['a': i64, 'b': i64], struct['b': i64, 'a': i64] }
```

```zest-test
[a: 1, b: 2] == [b: 100, a: 1]

Cannot call zest.Builtin.equal with these args: { ['a': 1, 'b': 2], ['b': 100, 'a': 1] }

Cannot call zest.Builtin.equal with these args: { struct['a': i64, 'b': i64], struct['b': i64, 'a': i64] }
```

```zest-test
[a: 1, b: 2] == [b: 2, a: 1, c: 3]

Cannot call zest.Builtin.equal with these args: { ['a': 1, 'b': 2], ['b': 2, 'a': 1, 'c': 3] }

Cannot call zest.Builtin.equal with these args: { struct['a': i64, 'b': i64], struct['b': i64, 'a': i64, 'c': i64] }
```

```zest-test
[a: 1, b: 2] == [b: 2, a: 1]/map[string, i64]

Name not bound: map
```

```zest-test
[a: 1, b: 2] != [b: 2, a: 1]/map[string, i64]

Name not bound: map
```

```zest-test
[nums: 42]/union[strings: string, nums: i64] ~= [nums: 42]/struct[nums: i64]

TODO eval: dir.ExprData{ .call_builtin = zest.Builtin.equivalent }

TODO infer: dir.ExprData{ .call_builtin = zest.Builtin.equivalent }
```

```zest-test
struct[name: string, age: i64] == struct[name: string, age: i64]

Cannot call zest.Builtin.equal with these args: { struct['name': string, 'age': i64], struct['name': string, 'age': i64] }

TODO infer: dir.ExprData{ .repr_string = void }
```

## ordering

TODO < to compare by type then data, ~< to compare by data only.