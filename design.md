## notation

Encoded as utf8.

TODO Booleans. Encode as `union[only["true"], only["false"]]`, but with builtin sugar.

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

Tokenizer error at 0: 'not a
string'
```

### objects

Zero or more key-value pairs, separated by commas.

```
[]

[]
```

```
['a': 'apple', 'b': 'bear']

[a: 'apple', b: 'bear']
```

Optional trailing comma.

```
['a': 'apple', 'b': 'bear',]

[a: 'apple', b: 'bear']
```

Key order doesn't matter. Keys may be printed in any order.

```
['b': 'bear', 'a': 'apple']

[a: 'apple', b: 'bear']
```

No repeated keys.

```
['a': 'apple', 'a': 'bear']

Duplicate key in map literal: 'a'
```

Omitted keys default to consecutive integers, starting at 0.

```
['a', 'b']

['a', 'b']
```

```
[0: 'a', 1: 'b']

['a', 'b']
```

Can mix omitted and present keys.

```
['a', 1: 'b', 'default': 'c']

['a', 'b', default: 'c']
```

But omitted keys must be written before present keys.

```
['a', 'default': 'c', 'b']

At 22. Positional elems must be before key/value elems
'b']
```

## representation

A value constists of a notation and a representation.

A representaton:
* Maps some subset of notations to bytes in memory.
* Constrains the available builtin operations on those notations.

Every notation has a default representation:
* Numbers default to i64, or f64 if they have digits after the decimal point.
  * TODO Add big-int and big-dec.
* Strings default to string.
* Objects default to structs.

When printing values, the representation can be omitted if it is the default representation for the notation, or if it determined by a surrounding representation.

### integers

```
i64[42]

42
```

```
i64[9223372036854775808]

At 4. Can't parse i64 because error.Overflow: 9223372036854775808
9223372036854775808]
```

```
i64[3.00]

3
```

```
i64[3.14]

Cannot convert 3.14 to i64
```

### floats

```
f64[42]

42.0
```

TODO Represent literals as big-int/big-dec to avoid this problem.

```
f64[9223372036854775808]

At 4. Can't parse i64 because error.Overflow: 9223372036854775808
9223372036854775808]
```

TODO Don't allow imprecise float parse.

```
f64[9223372036854775808.0]

9223372036854776000.0
```

```
f64[3.14]

3.14
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

Structs are objects with a fixed finite set of keys.

```
struct['a': i64, 'b': i64]['a': 0, 'b': 1]

[a: 0, b: 1]
```

```
struct['a': f64, 'b': i64]['a': 0, 'b': 1]

[a: 0.0, b: 1]
```

```
struct['a': f64, 'b': i64]['a': 0]

Cannot convert [a: 0] to struct['a': f64, 'b': i64]
```

```
struct['a': f64, 'b': i64]['a': 0, 'b': 1, 'c': 2]

Cannot convert [a: 0, b: 1, c: 2] to struct['a': f64, 'b': i64]
```

The keys are not required to be strings!

```
struct[f64, i64][0, 1]

[0.0, 1]
```

```
struct[['a', 'b']: string][['a', 'b']: 'c']

[['a', 'b']: 'c']
```

Structs are laid out contiguously in memory, in some implementation-defined order.

TODO May want to define key order for binary serialization?

Structs allow getting and setting keys, but not deleting or adding keys.

### lists

Lists are objects where the keys are consecutive integers beginning with 0 and the values all have the same representation.

```
list[f64][[]]

list[f64][[]]
```

```
list[f64][[0, 1, 2]]

list[f64][[0.0, 1.0, 2.0]]
```

```
list[string][[0, 1, 2]]

Cannot convert 0 to string
```

```
list[f64][['a': 'apple']]

Cannot convert [a: 'apple'] to list[f64]
```

```
list[f64][[1: 3.14]]

Cannot convert [1: 3.14] to list[f64]
```

Lists are laid out contiguously in memory, in key order.

Lists allow getting and setting keys, and pushing/popping.

(We don't need to distinguish between strings and string slices because we copy-on-write anyway).

### maps

Maps are objects with any number of entries, where all the keys have the same representation and all the values have the same representation.

```
map[i64, string][[0: 'zero', 1: 'one']]

map[i64, string][[0: 'zero', 1: 'one']]
```

```
map[i64, string][[0: 'zero', 1: 1]]

Cannot convert 1 to string
```

Maps are hashtables. Keys and values are stored inline.

Maps support getting/setting/adding/deleting keys.

TODO Make a decision about iteration order. Options:
* Implementation-defined. Breaks determinism.
* Sorted order. Expensive.
* Insertion order (like js maps). Then have to say that either key order matters in notation, or that equal maps can have different iteration orders.

### union

A union contains one of a fixed set of representations.

```
union[string, i64][42]

union[string, i64][42]
```

```
union[string, i64]['foo']

union[string, i64]['foo']
```

```
union[string, i64][3.14]

Cannot convert 3.14 to union[string, i64]
```

Unions are represented by an integer tag denoting the representation, followed by the representation of their value.

Unions support asking for the representation of their value and casting the value to a given representation.

### any

An any can represent any notation.

```
any[42]

any[42]
```

```
any['foo']

any['foo']
```

Anys are represented by a pointer to a value.

Anys support asking for the representation of their value and casting the value to a given representation.

### only

An only is an object with exactly one entry. Both the key and the value are stored in the representation itself.

```
only[42][]

only[42][]
```

Onlys are zero-sized.

Their main purpose is lifting values into representations where they are visible to specialization, type inference and compile-time computations.

```
only[42][]/get-repr

only[42]
```

```
only[42]/get-only

42
```

### repr repr

Representations are themselves notations. Their default representation is `repr`.

```
i64/get-repr

repr
```

The in-memory layout of `repr` is not exposed.

### as

The function `as` creates a new value with the same notation but a different representation.

```
42/as(f64)

42.0
```

If the new representation cannot encode the notation, `as` throws an error.

```
3.14/as(i64)

Cannot convert 3.14 to i64
```

## equality

Two __notations__ are equal if:
* They are both numbers, and they are equal (TODO scary can of worms).
* They are both strings, and they contain the same sequence of unicode characters.
* They are both objects, and:
  * They contain the same set of keys.
  * For each key, they contain the same value.

Two __values__ are `=` if they have the same representation and their notations are equal.

TODO Should it be a compile error to compare different reprs?

```
i64[42] = i64[42]

1
```

```
i64[42] = i64[1]

0
```

```
i64[42] = f64[42]

0
```

```
['a': 1, 'b': 2] = ['b': 2, 'a': 1]

1
```

```
['a': 1, 'b': 2] = ['b': 100, 'a': 1]

0
```

```
['a': 1, 'b': 2] = ['b': 2, 'a': 1, 'c': 3]

0
```

```
['a': 1, 'b': 2] = map[string, i64][['b': 2, 'a': 1]]

0
```

```
union[string, i64][42] = 42

0
```

Two __values__ are `~` if their notations are equal.

```
i64[42] ~ i64[42]

1
```

```
i64[42] ~ i64[1]

0
```

```
i64[42] ~ f64[42]

1
```

```
['a': 1, 'b': 2] ~ ['b': 2, 'a': 1]

1
```

```
['a': 1, 'b': 2] ~ ['b': 100, 'a': 1]

0
```

```
['a': 1, 'b': 2] ~ ['b': 2, 'a': 1, 'c': 3]

0
```

```
['a': 1, 'b': 2] ~ map[string, i64][['b': 2, 'a': 1]]

1
```

```
union[string, i64][42] ~ 42

1
```

TODO What about NaN?

TODO What about +0 vs -0.

## ordering

TODO < for type then notation, ~< for notation only

## names

Names start with a lowercase letter and can contain lowercase letters, numbers and hyphens.

TODO Assignment should return null/void.

```
some-name: 'foo'

0
```

```
side2side: 'foo'

0
```

```
NoCaps: 'foo'

Tokenizer error at 0: NoCaps: 'foo'
```

```
0digits-at-start: 'foo'

At 1. Expected Tokenizer.Token.eof, found Tokenizer.Token.name
digits-at-start: 'foo'
```

```
0-digits-at-start: 'foo'

At 1. Expected Tokenizer.Token.eof, found Tokenizer.Token.-
-digits-at-start: 'foo'
```

In objects, if a key is a valid name then it is treated as a string.

```
foo: 'the number';
[foo: 42]

[foo: 42]
```

Use brackets to treat the key as a name.

```
foo: 'the number';
[{foo}: 42]

['the number': 42]
```

If the key is a valid name then the value can be omitted and will be replaced by the value of that name.

```
foo: 'the number';
[foo:]

[foo: 'the number']
```

Names may not be shadowed.

```
foo: 1;
foo: 2

Name foo shadows earlier definition
```

```
foo: 1;
bar: () {
  foo: 2;
}
// TODO Name foo shadows earlier definition

0
```

```
foo: 1;
bar: (foo) 2
// TODO Name foo shadows earlier definition

0
```

## fields

The `get` function retrieves the value associated with a key in an object.

```
abc: [a: 1, b: 2, c: 3];
abc/get('b')

2
```

When the key is a string which is a valid name, this can be abbreviated to:

```
abc: [a: 1, b: 2, c: 3];
abc:b

2
```

If the key is not present in the map then an error is returned.

```
abc: [a: 1, b: 2, c: 3];
abc/get('d')

Key 'd' not found in [a: 1, b: 2, c: 3]
```

The `try-get` function instead returns an optional value.

```
abc: [a: 1, b: 2, c: 3];
abc/try-get('b')

[some: 2]
```

```
abc: [a: 1, b: 2, c: 3];
abc/try-get('d')

'none'
```

```
abc: [a: 1, b: 2, c: 3];
abc/try-get('b'):some

2
```

```
abc: [a: 1, b: 2, c: 3];
abc/try-get('d'):some

Cannot get key 'some' from non-object 'none'
```

## functions

```
foo: (x) x + 1;
foo(1)

2
```

Functions close over variables in their scope.

```
n: 1;
inc: (x) x + n;
inc(1)

2
```

Function definitions can only appear in variable definitions or as the argument to a function call.

```
foo: (x) x + 1

0
```

```
foo: {(x) x + 1}

Functions may only be defined at the top of definitions or call arguments
```

```
{(x) x + 1}(1)

Functions may only be defined at the top of definitions or call arguments
```

```
twice: (x, f) f(f(x));
twice(1, (x) x + 1);

3
```

```
twice: (x, f) f(f(x));
twice(1, if 1 (x) x + 1 else (x) x + 2);
3

Functions may only be defined at the top of definitions or call arguments
```

Function definitions immediately after a function call are interpreted as additional positional or named arguments to the function call.

```
twice: (x, f) f(f(x));
twice(1)
  (x) x + 1

3
```

```
twice: (x, f:) f(f(x));
twice(1)
  f: (x) x + 1

3
```

If a post-fix argument is a function with no arguments, then the `()` can be omitted:

TODO example requires mutation

Functions can only be referenced as the head of an argument call or the argument to a function call.

```
foo: () 42;
['nope': foo]

Functions may only be reference as the head of a call or an argument to a call
```

```
foo: () 42;
[{foo}: 'nope']

Functions may only be reference as the head of a call or an argument to a call
```

```
zero: () 0;
one: () 1;
if 1 one else zero

Functions may only be reference as the head of a call or an argument to a call
```

```
foo: () 42;
bar: () foo;
bar()

Functions may only be reference as the head of a call or an argument to a call
```

TODO patterns

TODO control flow capture

```
try: (body, catch:) body(throw: catch)
try()
  (throw:)
    throw('oh no!')
  catch: (error)
    return-to(try, [error:])

[error: 'oh no!']
```

TODO some way to indicate functions which always return?

## mutation

TODO mutable value semantics

## errors

TODO throw/panic as implicit arguments. try sets throw argument for body.

## syntax hunches

`[]` always indicates constructing a value. The result looks like the syntax.

`()` always indicates computation: either defining (`(x) x + 1`) or performing (`inc(x)`).

`{}` is used for grouping instead of `()` to avoid ambiguity.

`:` is used for definition to avoid ambiguity with equality (`=`) and assignment (TODO).

`foo:bar` is used for field access to mirror definition `foo: [bar: 42]`.

The syntax for function patterns is consistent with the syntax for creating structs - as if functions take a struct of arguments.

The syntax for passing function arguments outside a function is intended to allow builtin control flow (if/each/try etc) to look like function calls.

Parsing requires only 2-token lookahead (TODO verify once all tests pass).

I gave up on trying to avoid the shift key - there aren't enough good shiftless symbols to indicate all the major groups of syntax.

## misc

TODO We use value for both representation+notation and key/value. Think of a better name for the former.

TODO Calling syntax and destructuring that behaves like objects.
