```zest-test


[]

[TODO print]
```

```zest-test
42

42
```

```zest-test
1000001

1000001
```

```zest-test
1
2

2
```

```zest-test
a = 1
b = 2
a

1
```

```zest-test
a = 1
a = 2
a

Name already bound: a
```

```zest-test
[x: 1, yz: [y: 2, z: 3]].x

1
```

```zest-test
[x: 1, yz: [y: 2, z: 3]].yz.y

2
```

```zest-test
[x: 1, yz: [y: 2, z: 3]].yz.z

3
```

```zest-test
a = [x: 1, y: 2]
a.x

1
```

```zest-test
a = [x: 1, y: 2]
a.y

2
```

```zest-test
a = [[x: 1, y: 2], z: 3]
a.0.x

1
```

```zest-test
a = [[x: 1, y: 2], z: 3]
a.0.y

2
```

```zest-test
a = [[x: 1, y: 2], z: 3]
a.z

3
```

```zest-test
// TODO Should all constants be stageable?
k = 'x'
a = [{k}: 1]
a.x

1

Cannot unstage value: string
```

```zest-test
k = () 'x'
a = [{k()}: 1]
a.x

1
```

```zest-test
k = 'x'
k2 = () k
a = [{k2()}: 1]
a.x

1

Cannot unstage value: fun[39, k: string]
```

```zest-test
a = () 1
b = 2
a()

1
```

```zest-test
a = (x) x
b = 2
a(1)

1
```

```zest-test
a = (x, y) x
a(7,11)

7
```

```zest-test
a = (x, y) y
a(7,11)

11
```

```zest-test
a = (:x, :y) y
a(y: 7, x: 11)

7
```

```zest-test
a = (b) b.y
a([x: 11, y: 7])

7
```

```zest-test
a = (x, y) [:x, :y]
a(7, 11).x

7
```

```zest-test
a = (x, y) [:x, :y]
a(7, 11).y

11
```

```zest-test
a = 1
b = () a
b()

1
```

```zest-test
a = 1
b = () {
  c = () {
    d = () a
    d()
  }
  c()
}
b()

1
```

```zest-test
a = ([:x]) x
a([x: 1])

1
```

```zest-test
a = ([:x]) x
a(1)

Expected an object, found: 1

Expected an object, found: i64
```

```zest-test
f = () 1
a = [x: f()]
a.x

1
```

```zest-test
a = (x) [x,x]
b = (x) a([x,x])
b(1).0.0

1
```

```zest-test
a = (x) [x,x]
b = (x) a([x,x])
b(1).1.1

1
```

```zest-test
f = (x) x
a = f(1)
b = f(2)
a

1
```

```zest-test
f = (x) x
[f(0), f(1), f(2)].0

0
```

```zest-test
f = (x) x
[f(0), f(1), f(2)].1

1
```



```zest-test
f = (x) x
[f(0), f(1), f(2)].2

2
```

```zest-test
f = (x) x
a = [f(0), f(1)]
a.0

0
```

```zest-test
f = (x) x
a = [f(0), f(1)]
a.1

1
```

```zest-test
f = (x) x
f(1)
f([y: 1])
2

2
```


```zest-test
a mut = 1
a

1
```

```zest-test
a mut = 1
a@ = 2
a

2
```

```zest-test
a mut = [x: 1, y: 2]
a.x@ = 3
a.x

3
```

```zest-test
a mut = [x: 1, y: 2]
a.x@ = 3
a.y

2
```

```zest-test
a mut = [x: 1, y: 2]
a.y@ = 3
a.x

1
```

```zest-test
a mut = [x: 1, y: 2, z: 3]
a.y@ = 3
a.y

3
```

```zest-test
a mut = [x: 1, y: 2, z: 3]
a@ = [x: a.y, y: a.x, z: a.z]
a.x

2
```

```zest-test
a mut = [x: 1, y: 2, z: 3]
a@ = [x: a.y, y: a.x, z: a.z]
a.y

1
```

```zest-test
a mut = [x: 1, y: 2, z: 3]
a@ = [x: a.y, y: a.x, z: a.z]
a.z

3
```


```zest-test
a mut = [x: 1, y: 2]
a@ = 3
a

Expected struct[x: i64, y: i64], found i64
```

```zest-test
a mut = [x: 1, y: 2]
a.x@ = a
a.x.x

Expected i64, found struct[x: i64, y: i64]
```

```zest-test
a mut = 1
b = a
a@ = 2
b

1
```

```zest-test
a mut = [1]
b = a
a@ = [2]
b.0

1
```

```zest-test
p mut = [[0,1],[2,3]]
p@ = [[p.0.0, p.1.0], [p.0.1, p.1.1]]
p.0.1

2
```

```zest-test
p mut = [[0,1],[2,3]]
p@ = [[p.0.0, p.1.0], [p.0.1, p.1.1]]
p.1.0

1
```

```zest-test
f mut = (x) 101
[{f}: 1]

[[]/fun[39]: 1]

Cannot unstage value: ref[fun[39]]
```

```zest-test
a mut = 1
a@

Expected a value containing no mutable references, found: 1/ref[i64]

Expected a value containing no mutable references, found: ref[i64]
```

```zest-test
a mut = 1
[a@]

Expected a value containing no mutable references, found: [1/ref[i64]]

Expected a value containing no mutable references, found: struct[ref[i64]]
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

```zest-test
f = (x) x
a mut = 1
f(a@)

Expected a value containing no mutable references, found: 1/ref[i64]

Expected a value containing no mutable references, found: ref[i64]
```


```zest-test
f = (x mut) x
a mut = 1
f(a)

Expected a mutable reference, found: 1

Expected a mutable reference, found: i64
```

```zest-test
f = (x mut) x
a mut = 1
f(a@)

1
```

```zest-test
f = (x mut) {
  x@ = 2
}
a mut = 1
f(a@)
a

2
```

```zest-test
f = (a mut) {
  [x: a.y, y: a.x]
}
a mut = [x: 1, y: 2]
a@ = f(a@)
a.x

2
```

```zest-test
f = (a mut) {
  [x: a.y, y: a.x]
}
a mut = [x: 1, y: 2]
a@ = f(a@)
a.y

1
```

```zest-test
x mut = [0,1,2]
x@ = [x.2, x.1, x.0]
x.0

2
```

```zest-test
x mut = [0,1,2]
x@ = [x.2, x.1, x.0]
x.1

1
```

```zest-test
x mut = [0,1,2]
x@ = [x.2, x.1, x.0]
x.2

0
```

```zest-test
x mut = 0
y mut = 1
[x@, y@] = [y,x]
x

1
```


```zest-test
x mut = 0
y mut = 1
[x@, y@] = [y,x]
y

0
```

```zest-test
x mut = 0
f = () 1
x@ = f()
x

1
```

```zest-test
x mut = [0]
f = () [1]
x@ = f()
x.0

1
```

```zest-test
a mut = [42]
b = a
b.0

42
```

```zest-test
a mut = [0, 0]
b mut = [1, 1]
a@ = b
a.0

1
```

```zest-test
a mut = [42]
a
a.0

42
```

```zest-test
if 1 42 else 101

42
```

```zest-test
if 0 42 else 101

101
```

```zest-test
a mut = 0
if 1 {a@ = 42} else {a@ = 101}
a

42
```

```zest-test
a mut = 0
if 0 {a@ = 42} else {a@ = 101}
a

101
```

```zest-test
a mut = 0
b mut = {a@ = 42}
b@ = {c = 1}
a

42
```

```zest-test
1 + 2

3
```


```zest-test
1 + [2]

Cannot call zest.Builtin.add with these args: { 1, [2] }

Cannot call zest.Builtin.add with these args: { i64, struct[i64] }
```

```zest-test
a mut = 1
a@ = a + 2
a

3
```

```zest-test
{2 * 3} - 7

-1
```

```zest-test
a mut = 2
a@ = a * 3
a@ = a - 7
a

-1
```

```zest-test
a = 42
a == 43

0
```

```zest-test
a = 42
a == 42

1
```

```zest-test
a = 42
a != 42

0
```

```zest-test
a = 42
a != 43

1
```

```zest-test
a = 42
a < 42

0
```

```zest-test
a = 42
a < 43

1
```

```zest-test
a = -11
a + 25

14
```

```zest-test
-11 + 25

14
```

```zest-test
x mut = 0
while {x < 5} {
  x@ = x + 1
}
x

5
```

```zest-test
x = 5
// Don't generate this dead code
[x, x + 1].0 + 1
x

5
```

```zest-test
x mut = 5
// Don't generate this dead code
[x, x + 1].0 + 1
x

5
```

```zest-test
x mut = 5
// Don't generate this dead code
if 0 1 else [x, x + 1].0 + 1
x

5
```

```zest-test
x mut = 0
y mut = 1
if {x == y} 42 else 101

101
```

```zest-test
a mut = 42
x = {
  // block returns []
  a@ = 111
}
a

111
```

```zest-test
a mut = 42
inc = () { a@ = a + 1 }
inc()
inc()
a

44
```


```zest-test
a mut = 42
make-inc = () () { a@ = a + 1 }
inc = make-inc()
inc()
inc()
a

Expected a value containing no mutable references, found: [a: 42/ref[i64]]/fun[41, a: ref[i64]]

Expected a value containing no mutable references, found: fun[41, a: ref[i64]]
```

```zest-test
a mut = 42
get = () a
a@ = a + 1
get()

43
```

```zest-test
a mut = 42
get = () a
b mut = get

Expected a value containing no mutable references, found: [a: 42/ref[i64]]/fun[39, a: ref[i64]]

Expected a value containing no mutable references, found: fun[39, a: ref[i64]]
```

```zest-test
square-n-times = (m mut, n) {
    i mut = 0
    while {i < n} {
        m@ = [
            [
                {m.0.0 * m.0.0} + {m.0.1 * m.1.0},
                {m.0.0 * m.0.1} + {m.0.1 * m.1.1},
            ],
            [
                {m.1.0 * m.0.0} + {m.1.1 * m.1.0},
                {m.1.0 * m.0.1} + {m.1.1 * m.1.1},
            ],
        ]
        i@ = i + 1
    }
}
m mut = [[0,1],[1,0]]
square-n-times(m@, 3)
0

0
```

```zest-test
a = a

Name not bound: a
```

```zest-test
a mut = a

Name not bound: a
```

```zest-test
while 0 []
1

1
```

```zest-test
--101

101
```

```zest-test
pass = (c) c
pass([1])
pass([1,2])
pass([1,2,3]).2

3
```

```zest-test
f = (opts: [:y, :z]) z
f(opts: [y: 1, z: 2])

2
```

```zest-test
f = (opts: [:y, :z]) z
opts = [y: 1, z: 2]
f(:opts)

2
```

```zest-test
f = (:opts) opts.z
f(opts: [y: 1, z: 2])

2
```

```zest-test
f = (:opts) opts.z
opts = [y: 1, z: 2]
f(:opts)

2
```

```zest-test
i64
string
repr
42

42

TODO infer: dir.ExprData{ .repr_i64 = void }
```

```zest-test
i64 = 1

Name already bound: i64
```

```zest-test
[i64: 101].i64

101
```

```zest-test
[{i64}: 101].{i64}

101
```

```zest-test
f = (i64) 42
f(0)

Name already bound: i64
```

```zest-test
struct
only
42

42

TODO infer: dir.ExprData{ .repr_kind_struct = void }
```

```zest-test
i64[42]

42
```

```zest-test
f = (x) i64[x]
f(42)

42
```

```zest-test
a = struct[x: i64, y: i64][[x: 42, y: 101]]
a.y

101
```

```zest-test
a = struct[x: i64, y: i64][[x: 42, y: []]]
a.y

Expected struct[x: i64, y: i64], found struct[x: i64, y: struct[]]

Expected i64, found struct[]
```

```zest-test
a = struct[x: i64, y: i64][[x: 42, z: 101]]
a.y

Expected struct[x: i64, y: i64], found struct[x: i64, z: i64]
```

```zest-test
a = struct[x: i64, y: i64][[x: 42, y: 101, z: 0]]
a.y

Expected struct[x: i64, y: i64], found struct[x: i64, y: i64, z: i64]
```

```zest-test
a mut = [42]
// Pointless copy here because make expr blocks dest
b = struct[struct[i64]][[a]]
b.0.0

42
```

```zest-test
%memory-size(42)

Parse error: expected 0 arguments, found 1 arguments
At 1:16:
%memory-size(42)
                ^
```

```zest-test
%memory-size()

1/u32

129/u32
```

```zest-test
%memory-grow(u32[3])

1/u32

129/u32
```

```zest-test
%memory-grow(u32[3])
%memory-grow(u32[4])

4/u32

132/u32
```

```zest-test
%memory-grow(u32[3])
%memory-grow(u32[4])
%memory-size()

8/u32

136/u32
```

```zest-test
%heap-start()

42/u32

8388781/u32
```

```zest-test
%size-of(i64)

8/u32
```

```zest-test
%size-of(struct[i64, struct[u32, i64]])

20/u32
```

```zest-test
%memory-grow(u32[1])
%load(%heap-start() + u32[0], i64)

0
```

```zest-test
%memory-grow(u32[1])
%store(%heap-start() + u32[0], 42)
%store(%heap-start() + %size-of(i64), 101)
%load(%heap-start() + u32[0], i64)

42
```

```zest-test
%memory-grow(u32[1])
%store(%heap-start() + u32[0], 42)
%store(%heap-start() + %size-of(i64), 101)
%load(%heap-start() + %size-of(i64), i64)

101
```

```zest-test
%memory-grow(u32[1])
%store(%heap-start() + u32[1], 42)
%load(%heap-start() + u32[0], i64)

10752
```

```zest-test
%memory-grow(u32[1])
%store(%heap-start(), [0,[1,2]])
x = %load(%heap-start(), struct[i64, struct[i64, i64]])
x.0

0
```

```zest-test
%memory-grow(u32[1])
%store(%heap-start(), [0,[1,2]])
x = %load(%heap-start(), struct[i64, struct[i64, i64]])
x.1.0

1
```

```zest-test
%memory-grow(u32[1])
%store(%heap-start(), [0,[1,2]])
x = %load(%heap-start(), struct[i64, struct[i64, i64]])
x.1.1

2
```

```zest-test
%memory-grow(u32[1])
x = [%load(%heap-start(), i64), %store(%heap-start(), 42), %load(%heap-start(), i64)]
x.0 + x.2

42
```

```zest-test
%memory-grow(u32[1])
%store(%heap-start(), [1,2,3,4])
%memory-fill(%heap-start() + %size-of(i64), u32[0], u32[2] * %size-of(i64))
x = %load(%heap-start(), struct[i64,i64,i64,i64])
x.0 + x.1 + x.2 + x.3

5
```

```zest-test
%memory-grow(u32[1])
%store(%heap-start(), [1,2,3,4])
%memory-copy(%heap-start() + {u32[2] * %size-of(i64)}, %heap-start(), u32[4] * %size-of(i64))
x = %load(%heap-start(), struct[i64,i64,i64,i64])
x.0 + x.1 + x.2 + x.3

6
```

```zest-test
%memory-grow(u32[1])
%store(%heap-start(), [1,2,3,4])
%memory-copy(%heap-start(), %heap-start() + {u32[2] * %size-of(i64)}, u32[4] * %size-of(i64))
x = %load(%heap-start(), struct[i64,i64,i64,i64])
x.0 + x.1 + x.2 + x.3

7
```

```zest-test
inc = (x) x + 1
1/inc()

2
```

```zest-test
add = (x, y) x + y
1/add(2)

3
```

```zest-test
x mut = 1
inc = (y mut) { y@ = y + 1 }
x@/inc()
x

2
```

```zest-test
x mut = [1]
inc = (y mut) { y@ = y + 1 }
x.0@/inc()
x.0

2
```

```zest-test
42 + 1/i64

43
```

```zest-test
42 + 1/u32

Cannot call zest.Builtin.add with these args: { 42, 1/u32 }

Cannot call zest.Builtin.add with these args: { i64, u32 }
```

```zest-test
x/i64 = 42
x + 1

43
```

```zest-test
x/u32 = 42
x + 1

Cannot call zest.Builtin.add with these args: { 42/u32, 1 }

Cannot call zest.Builtin.add with these args: { u32, i64 }
```

```zest-test
f = (x/i64) x + 1
f(1)

2
```

```zest-test
f = (x/i64) x + 1
f(1/u32)

Expected i64, found u32
```

```zest-test
f = (:x/i64) x + 1
x = 1
f(:x/i64)

2
```

```zest-test
f = (:x/i64) x + 1
x = 1/u32
f(:x/u32)

Expected i64, found u32
```

```zest-test
f = (x/struct[i64, i64]) x.1
f([101,42])

42
```

```zest-test
f = (x/struct[i64, i64]) x.1
f([101,42,32])

Expected struct[i64, i64], found struct[i64, i64, i64]
```

```zest-test
x = 0
f = () [x][]
f()

Cannot make [0]

Cannot unstage value: i64
```

```zest-test
'foo'

'foo'
```

```zest-test
'bar'
'foo'

'foo'
```

```zest-test
x = ['bar','foo']
x.1

'foo'
```

```zest-test
%print('foo')
42

foo42
```

```zest-test
x = [{%print('foo')}: 42]
x.{%print('bar')}

Tried to perform a side effect during pure evaluation: zest.Builtin.print
```

```zest-test
%panic()

panic

RuntimeError: unreachable
    at <anonymous> (wasm://wasm/f7decaaa:1:197)
    at <anonymous> (wasm://wasm/f7decaaa:1:177)
    at file:///home/jamie/zest/test.js:33:24
```

```zest-test
panic = (message) {
  %print(message)
  %print('\n')
  %panic()
}
panic('Oh no')

Oh no
panic

Oh no
RuntimeError: unreachable
    at <anonymous> (wasm://wasm/442ca1a2:1:307)
    at <anonymous> (wasm://wasm/442ca1a2:1:222)
    at <anonymous> (wasm://wasm/442ca1a2:1:178)
    at file:///home/jamie/zest/test.js:33:24
```

```zest-test
%less-than(3, 7)

1
```

```zest-test
42 % 0

Division by zero

RuntimeError: remainder by zero
    at <anonymous> (wasm://wasm/04a22482:1:192)
    at <anonymous> (wasm://wasm/04a22482:1:177)
    at file:///home/jamie/zest/test.js:33:24
```

```zest-test
1 % 3

1
```

```zest-test
7 % 3

1
```

```zest-test
-1 % 3

-1
```

```zest-test
-4 % 3

-1
```

```zest-test
u32[1] << u32[10]

1024/u32
```

```zest-test
1 << 10

Cannot call zest.Builtin.bit-shift-left with these args: { 1, 10 }

Cannot call zest.Builtin.bit-shift-left with these args: { i64, i64 }
```

```zest-test
u32[1] << u32[32]

1/u32
```

```zest-test
foo = () {
  bad grammar
}

Parse error: expected }, found sir.TokenData.name
At 2:13:
  bad grammar
             ^
```

```zest-test
4294967295

4294967295
```

```zest-test
u32[4294967295]

4294967295/u32

-1/u32
```

```zest-test
%clz(u32[0])

32/u32
```

```zest-test
%clz(u32[1])

31/u32
```

```zest-test
%clz(u32[2])

30/u32
```

```zest-test
%clz(u32[3])

30/u32
```

```zest-test
union[][4]
1

Expected union[], found i64
```

```zest-test
union[4][4]

Cannot make union with these args: [4]
```

```zest-test
a mut = union[some: i64, none: struct[]][[some: 42]]
b mut = union[some: i64, none: struct[]][[none: []]]
c mut = 101
1

1
```

```zest-test
a = union[some: i64, none: struct[]][[some: 42]]
a.some

42
```

```zest-test
a = union[some: i64, none: struct[]][[some: 42]]
a.none

Key 'none' not found in [some: 42]/union[some: i64, none: struct[]]

RuntimeError: unreachable
    at <anonymous> (wasm://wasm/4956a59a:1:186)
    at <anonymous> (wasm://wasm/4956a59a:1:177)
    at file:///home/jamie/zest/test.js:33:24
```

```zest-test
a mut = union[some: i64, none: struct[]][[some: 42]]
a.some

42
```

```zest-test
a mut = union[some: i64, none: struct[]][[some: 42]]
a.none

Key 'none' not found in [some: 42]/union[some: i64, none: struct[]]

RuntimeError: unreachable
    at <anonymous> (wasm://wasm/6ea0375a:1:223)
    at <anonymous> (wasm://wasm/6ea0375a:1:177)
    at file:///home/jamie/zest/test.js:33:24
```

```zest-test
a = union[some: i64, none: struct[]][[none: []]]
a.some

Key 'some' not found in [none: []]/union[some: i64, none: struct[]]

RuntimeError: unreachable
    at <anonymous> (wasm://wasm/484cb326:1:188)
    at <anonymous> (wasm://wasm/484cb326:1:177)
    at file:///home/jamie/zest/test.js:33:24
```

```zest-test
a = union[some: i64, none: struct[]][[none: []]]
a.none

[]

[TODO print]
```

```zest-test
a mut = union[some: i64, none: struct[]][[none: []]]
a.some

Key 'some' not found in [none: []]/union[some: i64, none: struct[]]

RuntimeError: unreachable
    at <anonymous> (wasm://wasm/413640ae:1:218)
    at <anonymous> (wasm://wasm/413640ae:1:177)
    at file:///home/jamie/zest/test.js:33:24
```

```zest-test
a mut = union[some: i64, none: struct[]][[none: []]]
a.none

[]

[TODO print]
```

```zest-test
// TODO How should infallible patterns treat unions?
f = ([some: x]) x
f(union[some: i64, none: struct[]][[some: 42]])

TODO eval: dir.ExprData{ .assert_object = dir.ExprData__struct_24397{ .count = 1 } }

TODO infer: dir.ExprData{ .i64 = 0 }
```

```zest-test
a mut = union[some: i64, none: struct[]][[some: 42]]
a.some@ = a.some + 1
a.some

43
```

```zest-test
// TODO Borrow check shouldn't allow this.
a mut = union[some: i64, none: i64][[some: 42]]
a.some@ = {
  a@ = union[some: i64, none: i64][[none: 101]]
  42
}
a.none

Key 'some' not found in [none: 101]/union[some: i64, none: i64]

RuntimeError: unreachable
    at <anonymous> (wasm://wasm/fbc69476:1:240)
    at <anonymous> (wasm://wasm/fbc69476:1:177)
    at file:///home/jamie/zest/test.js:33:24
```

```zest-test
a = union[some: i64, none: struct[]][[some: 42]]
%union-has-key(a, 'some')

1
```

```zest-test
a = union[some: i64, none: struct[]][[some: 42]]
%union-has-key(a, 'none')

0
```

```zest-test
a = union[some: i64, none: struct[]][[some: 42]]
%union-has-key(a, 'many')

Can never find key 'many' in [some: 42]/union[some: i64, none: struct[]]

Can never find key 'many' in union[some: i64, none: struct[]]
```

```zest-test
a mut = union[some: i64, none: struct[]][[some: 42]]
%union-has-key(a, 'some')

1
```

```zest-test
a mut = union[some: i64, none: struct[]][[some: 42]]
%union-has-key(a, 'none')

0
```

```zest-test
a = 42
%repr-of(a)
101

101

TODO infer: dir.ExprData{ .repr_of = void }
```

```zest-test
a = 42
%repr-of(a + 1)
101

101

TODO infer: dir.ExprData{ .repr_of = void }
```

```zest-test
a = 42
b = [{%repr-of(a); 'x'}: 101]
b.x

101
```

```zest-test
a = 42
b = [{%repr-of(a + 1); 'x'}: 101]
b.x

101
```

```zest-test
// TODO Don't allow side-effects in staged eval.
a = 42
b = [{%print('surprise')}: 101]
b.{[]}

Tried to perform a side effect during pure evaluation: zest.Builtin.print
```

```zest-test
%union-has-key(%reflect(%repr-of(42)), 'i64')

1

TODO infer: dir.ExprData{ .call_builtin = zest.Builtin.reflect }
```

```zest-test
// No type error from false branch because condition is comptime-known
a = only[1][[]]
if a 42 else 'oh no'

42
```

```zest-test
// No type error from true branch because condition is comptime-known
a = only[0][[]]
if a 'oh no' else 101

101
```

```zest-test
a = only[42][[]]
i64[a]

42
```

```zest-test
a = [x: 42]
t = only[%reflect(%repr-of(a))][[]]
if {only[%union-has-key(%from-only(t), 'struct')][[]]} 101 else 202

101
```

```zest-test
a = [x: 42]
t = only[%reflect(%repr-of(a))][[]]
if {only[%union-has-key(%from-only(t), 'union')][[]]} 101 else 202

202
```

```zest-test
%print(42)
101

42101
```

```zest-test
if {%print('ok'); only[1][[]]} 42 else 101

ok42
```

```zest-test
while { only[0][[]] } { %print('ok') }
42

42
```

```zest-test
%each(1, (k, v) %print(k))

Cannot call zest.Builtin.each with these args: { 1, []/fun[39] }

Cannot call zest.Builtin.each with these args: { i64, fun[39] }
```

```zest-test
// TODO Add a char type
%each('hello world', (k, v) %print(k))

Cannot call zest.Builtin.each with these args: { 'hello world', []/fun[39] }

Cannot call zest.Builtin.each with these args: { string, fun[39] }
```

```zest-test
s = [a: 1, b: 2]
%each(s, (k, v) {
  %print(%from-only(k))
  %print('\n')
  %print(v)
  %print('\n')
})
101

a
1
b
2
101
```

```zest-test
s mut = [a: 1, b: 2]
%each(s, (k, v) {
  %print(%from-only(k))
  %print('\n')
  %print(v)
  %print('\n')
})
101

a
1
b
2
101
```

```zest-test
u = union[a: i64, b: i64][[a: 1]]
%each(u, (k, v) {
  %print(%from-only(k))
  %print('\n')
  %print(v)
  %print('\n')
})
101

a
1
101
```

```zest-test
u = union[a: i64, b: i64][[b: 2]]
%each(u, (k, v) {
  %print(%from-only(k))
  %print('\n')
  %print(v)
  %print('\n')
})
101

b
2
101
```

```zest-test
u mut = union[a: i64, b: i64][[a: 1]]
%each(u, (k, v) {
  %print(%from-only(k))
  %print('\n')
  %print(v)
  %print('\n')
})
101

a
1
101
```

```zest-test
u mut = union[a: i64, b: i64][[b: 2]]
%each(u, (k, v) {
  %print(%from-only(k))
  %print('\n')
  %print(v)
  %print('\n')
})
101

b
2
101
```

```zest-test
print = (x) {
  t = only[%reflect(%repr-of(x))][[]]
  if {only[%union-has-key(%from-only(t), 'u32')][[]]} {
    %print(x)
  } else if {only[%union-has-key(%from-only(t), 'i64')][[]]} {
    %print(x)
  } else if {only[%union-has-key(%from-only(t), 'string')][[]]} {
    %print('\'')
    // TODO escape
    %print(x)
    %print('\'')
  } else {
    %panic()
  }
  %print('\n')
}
print(u32[42])
print(4294967295)
print('foo')
101

42
4294967295
'foo'
101
```

```zest-test
f = (a)/i64 a.x
f([x: 0])

0
```

```zest-test
f = (a)/i64 a.x
f([x: 'x'])

Expected i64, found string
```

```zest-test
[name: 0, 'still-a-name': 1, ' not': 2, '0not': 2]

[name: 0, still-a-name: 1, ' not': 2, '0not': 2]

[TODO print]
```

```zest-test
ns = namespace{
  maybe-int = union[some: i64, none: struct[]]
  f = (x) /maybe-int {
    if {x == 0} [none: []] else [some: x]
  }
}
ns..f(3).some

3
```

```zest-test
// TODO This requires some kind of peer-type union in `propagate`
ns = namespace{
  maybe-int = union[some: i64, none: struct[]]
  f = (x) {
    if {x == 0} [none: []]/maybe-int else [some: x]
  }
}
ns..f(3).some

3

Expected union[some: i64, none: struct[]], found struct[some: i64]
```

```zest-test
only[42][[]]

[]/only[42]
```

```zest-test
() 1

[]/fun[39]

[TODO print]/fun[TODO print]
```

```zest-test
x = ['a', 'b', 'c']/list[string]
x.1

'b'

Expected an object, found: list[string]
```

```zest-test
y = []/only[['a', 'b', 'c']]
%each(y, (k, v) {
  %print(%from-only(k))
  %print(' = ')
  %print(%from-only(v))
  %print('\n')
})

0 = a
1 = b
2 = c
[]

0 = a
1 = b
2 = c
[TODO print]
```

```zest-test
y = []/only[[a: 1]/union[a: i64, b: string]]
%each(y, (k, v) {
  %print(%from-only(k))
  %print(' = ')
  %print(%from-only(v))
  %print('\n')
})

a = 1
[]

a = 1
[TODO print]
```

```zest-test
y = []/only[['a', 'b', 'c']/list[string]]
%each(y, (k, v) {
  %print(%from-only(k))
  %print(' = ')
  %print(%from-only(v))
  %print('\n')
})

-1 = a
-1 = b
-1 = c
[]

0 = a
1 = b
2 = c
[TODO print]
```