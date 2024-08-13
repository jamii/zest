```


[]

undefined
```

```
42

42

42
```

```
1000001

1000001

1000001
```

```
1
2

2

2
```

```
a = 1
b = 2
a

1

1
```

```
a = 1
a = 2
a

Name already bound: a

Name already bound: a
```

```
[x: 1, yz: [y: 2, z: 3]].x

1

1
```

```
[x: 1, yz: [y: 2, z: 3]].yz.y

2

2
```

```
[x: 1, yz: [y: 2, z: 3]].yz.z

3

3
```

```
a = [x: 1, y: 2]
a.x

1

1
```

```
a = [x: 1, y: 2]
a.y

2

2
```

```
a = [[x: 1, y: 2], z: 3]
a.0.x

1

1
```

```
a = [[x: 1, y: 2], z: 3]
a.0.y

2

2
```

```
a = [[x: 1, y: 2], z: 3]
a.z

3

3
```

```
// TODO Should all constants be stageable?
k = 'x'
a = [{k}: 1]
a.x

1

Cannot unstage value: string
```

```
k = () 'x'
a = [{k()}: 1]
a.x

1

1
```

```
k = 'x'
k2 = () k
a = [{k2()}: 1]
a.x

1

Cannot unstage value: fun[id: 0, closure: struct['k': string]]
```

```
a = () 1
b = 2
a()

1

1
```

```
a = (x) x
b = 2
a(1)

1

1
```

```
a = (x, y) x
a(7,11)

7

7
```

```
a = (x, y) y
a(7,11)

11

11
```

```
a = (:x, :y) y
a(y: 7, x: 11)

7

7
```

```
a = (b) b.y
a([x: 11, y: 7])

7

7
```

```
a = (x, y) [:x, :y]
a(7, 11).x

7

7
```

```
a = (x, y) [:x, :y]
a(7, 11).y

11

11
```

```
a = 1
b = () a
b()

1

1
```

```
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

1
```

```
a = ([:x]) x
a([x: 1])

1

1
```

```
a = ([:x]) x
a(1)

Expected an object, found: 1

Expected an object, found: i64
```

```
f = () 1
a = [x: f()]
a.x

1

1
```

```
a = (x) [x,x]
b = (x) a([x,x])
b(1).0.0

1

1
```

```
a = (x) [x,x]
b = (x) a([x,x])
b(1).1.1

1

1
```

```
f = (x) x
a = f(1)
b = f(2)
a

1

1
```

```
f = (x) x
[f(0), f(1), f(2)].0

0

0
```

```
f = (x) x
[f(0), f(1), f(2)].1

1

1
```



```
f = (x) x
[f(0), f(1), f(2)].2

2

2
```

```
f = (x) x
a = [f(0), f(1)]
a.0

0

0
```

```
f = (x) x
a = [f(0), f(1)]
a.1

1

1
```

```
f = (x) x
f(1)
f([y: 1])
2

2

2
```


```
a mut = 1
a

1

1
```

```
a mut = 1
a@ = 2
a

2

2
```

```
a mut = [x: 1, y: 2]
a.x@ = 3
a.x

3

3
```

```
a mut = [x: 1, y: 2]
a.x@ = 3
a.y

2

2
```

```
a mut = [x: 1, y: 2]
a.y@ = 3
a.x

1

1
```

```
a mut = [x: 1, y: 2, z: 3]
a.y@ = 3
a.y

3

3
```

```
a mut = [x: 1, y: 2, z: 3]
a@ = [x: a.y, y: a.x, z: a.z]
a.x

2

2
```

```
a mut = [x: 1, y: 2, z: 3]
a@ = [x: a.y, y: a.x, z: a.z]
a.y

1

1
```

```
a mut = [x: 1, y: 2, z: 3]
a@ = [x: a.y, y: a.x, z: a.z]
a.z

3

3
```


```
a mut = [x: 1, y: 2]
a@ = 3
a

Expected struct['x': i64, 'y': i64], found i64

Expected struct['x': i64, 'y': i64], found i64
```

```
a mut = [x: 1, y: 2]
a.x@ = a
a.x.x

Expected i64, found struct['x': i64, 'y': i64]

Expected i64, found struct['x': i64, 'y': i64]
```

```
a mut = 1
b = a
a@ = 2
b

1

1
```

```
a mut = [1]
b = a
a@ = [2]
b.0

1

1
```

```
p mut = [[0,1],[2,3]]
p@ = [[p.0.0, p.1.0], [p.0.1, p.1.1]]
p.0.1

2

2
```

```
p mut = [[0,1],[2,3]]
p@ = [[p.0.0, p.1.0], [p.0.1, p.1.1]]
p.1.0

1

1
```

```
f mut = (x) 101
[{f}: 1]

[fun[id: 0, closure: struct[]][]: 1]

Cannot unstage value: ref[fun[id: 0, closure: struct[]]]
```

```
a mut = 1
a@

Expected a value containing no mutable references, found: ref[i64][1]

Expected a value containing no mutable references, found: ref[i64]
```

```
a mut = 1
[a@]

Expected a value containing no mutable references, found: [ref[i64][1]]

Expected a value containing no mutable references, found: struct[ref[i64]]
```


```
a mut = 1
b = a@

Expected a value containing no mutable references, found: ref[i64][1]

Expected a value containing no mutable references, found: ref[i64]
```

```
a mut = 1
b = [a@]

Expected a value containing no mutable references, found: [ref[i64][1]]

Expected a value containing no mutable references, found: struct[ref[i64]]
```

```
f = (x) x
a mut = 1
f(a@)

Expected a value containing no mutable references, found: ref[i64][1]

Expected a value containing no mutable references, found: ref[i64]
```


```
f = (x mut) x
a mut = 1
f(a)

Expected a mutable reference, found: 1

Expected a mutable reference, found: i64
```

```
f = (x mut) x
a mut = 1
f(a@)

1

1
```

```
f = (x mut) {
  x@ = 2
}
a mut = 1
f(a@)
a

2

2
```

```
f = (a mut) {
  [x: a.y, y: a.x]
}
a mut = [x: 1, y: 2]
a@ = f(a@)
a.x

2

2
```

```
f = (a mut) {
  [x: a.y, y: a.x]
}
a mut = [x: 1, y: 2]
a@ = f(a@)
a.y

1

1
```

```
x mut = [0,1,2]
x@ = [x.2, x.1, x.0]
x.0

2

2
```

```
x mut = [0,1,2]
x@ = [x.2, x.1, x.0]
x.1

1

1
```

```
x mut = [0,1,2]
x@ = [x.2, x.1, x.0]
x.2

0

0
```

```
x mut = 0
y mut = 1
[x@, y@] = [y,x]
x

1

1
```


```
x mut = 0
y mut = 1
[x@, y@] = [y,x]
y

0

0
```

```
x mut = 0
f = () 1
x@ = f()
x

1

1
```

```
x mut = [0]
f = () [1]
x@ = f()
x.0

1

1
```

```
a mut = [42]
b = a
b.0

42

42
```

```
a mut = [0, 0]
b mut = [1, 1]
a@ = b
a.0

1

1
```

```
a mut = [42]
a
a.0

42

42
```

```
if 1 42 else 101

42

42
```

```
if 0 42 else 101

101

101
```

```
a mut = 0
if 1 {a@ = 42} else {a@ = 101}
a

42

42
```

```
a mut = 0
if 0 {a@ = 42} else {a@ = 101}
a

101

101
```

```
a mut = 0
b mut = {a@ = 42}
b@ = {c = 1}
a

42

42
```

```
1 + 2

3

3
```


```
1 + [2]

Cannot call zest.Builtin.add with these args: { 1, [2] }

Cannot call zest.Builtin.add with these args: { i64, struct[i64] }
```

```
a mut = 1
a@ = a + 2
a

3

3
```

```
{2 * 3} - 7

-1

-1
```

```
a mut = 2
a@ = a * 3
a@ = a - 7
a

-1

-1
```

```
a = 42
a == 43

0

0
```

```
a = 42
a == 42

1

1
```

```
a = 42
a != 42

0

0
```

```
a = 42
a != 43

1

1
```

```
a = 42
a < 42

0

0
```

```
a = 42
a < 43

1

1
```

```
a = -11
a + 25

14

14
```

```
-11 + 25

14

14
```

```
x mut = 0
while {x < 5} {
  x@ = x + 1
}
x

5

5
```

```
x = 5
// Don't generate this trash
[x, x + 1].0 + 1
x

5

5
```

```
x mut = 5
// Don't generate this trash
[x, x + 1].0 + 1
x

5

5
```

```
x mut = 5
// Don't generate this trash
if 0 1 else [x, x + 1].0 + 1
x

5

5
```

```
x mut = 0
y mut = 1
if {x == y} 42 else 101

101

101
```

```
a mut = 42
x = {
  // block returns []
  a@ = 111
}
a

111

111
```

```
a mut = 42
inc = () { a@ = a + 1 }
inc()
inc()
a

44

44
```


```
a mut = 42
make-inc = () () { a@ = a + 1 }
inc = make-inc()
inc()
inc()
a

Expected a value containing no mutable references, found: fun[id: 1, closure: struct['a': ref[i64]]]['a': ref[i64][42]]

Expected a value containing no mutable references, found: fun[id: 1, closure: struct['a': ref[i64]]]
```

```
a mut = 42
get = () a
a@ = a + 1
get()

43

43
```

```
a mut = 42
get = () a
b mut = get

Expected a value containing no mutable references, found: fun[id: 0, closure: struct['a': ref[i64]]]['a': ref[i64][42]]

Expected a value containing no mutable references, found: fun[id: 0, closure: struct['a': ref[i64]]]
```

```
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

0
```

```
a = a

Name not bound: a

Name not bound: a
```

```
a mut = a

Name not bound: a

Name not bound: a
```

```
while 0 []
1

1

1
```

```
--101

101

101
```

```
pass = (c) c
pass([1])
pass([1,2])
pass([1,2,3]).2

3

3
```

```
f = (opts: [:y, :z]) z
f(opts: [y: 1, z: 2])

2

2
```

```
f = (opts: [:y, :z]) z
opts = [y: 1, z: 2]
f(:opts)

2

2
```

```
f = (:opts) opts.z
f(opts: [y: 1, z: 2])

2

2
```

```
f = (:opts) opts.z
opts = [y: 1, z: 2]
f(:opts)

2

2
```

```
i64
string
repr
42

42

TODO infer: dir.ExprData{ .repr_i64 = void }
```

```
i64 = 1

Name already bound: i64

Name already bound: i64
```

```
[i64: 101].i64

101

101
```

```
[{i64}: 101].{i64}

101

101
```

```
f = (i64) 42
f(0)

Name already bound: i64

Name already bound: i64
```

```
struct
only
42

42

TODO infer: dir.ExprData{ .repr_kind_struct = void }
```

```
i64[42]

42

42
```

```
f = (x) i64[x]
f(42)

42

42
```

```
a = struct[x: i64, y: i64][[x: 42, y: 101]]
a.y

101

101
```

```
a = struct[x: i64, y: i64][[x: 42, y: []]]
a.y

Expected struct['x': i64, 'y': i64], found struct['x': i64, 'y': struct[]]

Expected struct['x': i64, 'y': i64], found struct['x': i64, 'y': struct[]]
```

```
a = struct[x: i64, y: i64][[x: 42, z: 101]]
a.y

Expected struct['x': i64, 'y': i64], found struct['x': i64, 'z': i64]

Expected struct['x': i64, 'y': i64], found struct['x': i64, 'z': i64]
```

```
a = struct[x: i64, y: i64][[x: 42, y: 101, z: 0]]
a.y

Expected struct['x': i64, 'y': i64], found struct['x': i64, 'y': i64, 'z': i64]

Expected struct['x': i64, 'y': i64], found struct['x': i64, 'y': i64, 'z': i64]
```

```
a mut = [42]
// Pointless copy here because make expr blocks dest
b = struct[struct[i64]][[a]]
b.0.0

42

42
```

```
%memory-size(42)

%memory-size expected 0 arguments, found 1

%memory-size expected 0 arguments, found 1
```

```
%memory-size()

1

129
```

```
%memory-grow(u32[3])

1

129
```

```
%memory-grow(u32[3])
%memory-grow(u32[4])

4

132
```

```
%memory-grow(u32[3])
%memory-grow(u32[4])
%memory-size()

8

136
```

```
%heap-start()

42

8388650
```

```
%size-of(i64)

8

8
```

```
%size-of(struct[i64, struct[u32, i64]])

20

20
```

```
%memory-grow(u32[1])
%load(%heap-start() + u32[0], i64)

0

0
```

```
%memory-grow(u32[1])
%store(%heap-start() + u32[0], 42)
%store(%heap-start() + %size-of(i64), 101)
%load(%heap-start() + u32[0], i64)

42

42
```

```
%memory-grow(u32[1])
%store(%heap-start() + u32[0], 42)
%store(%heap-start() + %size-of(i64), 101)
%load(%heap-start() + %size-of(i64), i64)

101

101
```

```
%memory-grow(u32[1])
%store(%heap-start() + u32[1], 42)
%load(%heap-start() + u32[0], i64)

10752

10752
```

```
%memory-grow(u32[1])
%store(%heap-start(), [0,[1,2]])
x = %load(%heap-start(), struct[i64, struct[i64, i64]])
x.0

0

0
```

```
%memory-grow(u32[1])
%store(%heap-start(), [0,[1,2]])
x = %load(%heap-start(), struct[i64, struct[i64, i64]])
x.1.0

1

1
```

```
%memory-grow(u32[1])
%store(%heap-start(), [0,[1,2]])
x = %load(%heap-start(), struct[i64, struct[i64, i64]])
x.1.1

2

2
```

```
%memory-grow(u32[1])
x = [%load(%heap-start(), i64), %store(%heap-start(), 42), %load(%heap-start(), i64)]
x.0 + x.2

42

42
```

```
%memory-grow(u32[1])
%store(%heap-start(), [1,2,3,4])
%memory-fill(%heap-start() + %size-of(i64), u32[0], u32[2] * %size-of(i64))
x = %load(%heap-start(), struct[i64,i64,i64,i64])
x.0 + x.1 + x.2 + x.3

5

5
```

```
%memory-grow(u32[1])
%store(%heap-start(), [1,2,3,4])
%memory-copy(%heap-start() + {u32[2] * %size-of(i64)}, %heap-start(), u32[4] * %size-of(i64))
x = %load(%heap-start(), struct[i64,i64,i64,i64])
x.0 + x.1 + x.2 + x.3

6

6
```

```
%memory-grow(u32[1])
%store(%heap-start(), [1,2,3,4])
%memory-copy(%heap-start(), %heap-start() + {u32[2] * %size-of(i64)}, u32[4] * %size-of(i64))
x = %load(%heap-start(), struct[i64,i64,i64,i64])
x.0 + x.1 + x.2 + x.3

7

7
```

```
inc = (x) x + 1
1/inc()

2

2
```

```
add = (x, y) x + y
1/add(2)

3

3
```

```
x mut = 1
inc = (y mut) { y@ = y + 1 }
x@/inc()
x

2

2
```

```
x mut = [1]
inc = (y mut) { y@ = y + 1 }
x.0@/inc()
x.0

2

2
```

```
42 + 1/i64

43

43
```

```
42 + 1/u32

Cannot call zest.Builtin.add with these args: { 42, 1 }

Cannot call zest.Builtin.add with these args: { i64, u32 }
```

```
x/i64 = 42
x + 1

43

43
```

```
x/u32 = 42
x + 1

Cannot call zest.Builtin.add with these args: { 42, 1 }

Cannot call zest.Builtin.add with these args: { u32, i64 }
```

```
f = (x/i64) x + 1
f(1)

2

2
```

```
f = (x/i64) x + 1
f(1/u32)

Expected i64, found u32

Expected i64, found u32
```

```
f = (:x/i64) x + 1
x = 1
f(:x/i64)

2

2
```

```
f = (:x/i64) x + 1
x = 1/u32
f(:x/u32)

Expected i64, found u32

Expected i64, found u32
```

```
f = (struct[i64, i64][x]) x.1
f([101,42])

42

42
```

```
f = (struct[i64, i64][x]) x.1
f([101,42,32])

Expected struct[i64, i64], found struct[i64, i64, i64]

Expected struct[i64, i64], found struct[i64, i64, i64]
```

```
x = 0
f = () [x][]
f()

Cannot make [0]

Cannot unstage value: i64
```

```
'foo'

'foo'

undefined
```

```
'bar'
'foo'

'foo'

undefined
```

```
x = ['bar','foo']
x.1

'foo'

undefined
```

```
%print('foo')
42

foo42

foo42
```

```
x = [{%print('foo')}: 42]
x.{%print('bar')}

foobar42

42
```

```
%panic()

panic

RuntimeError: unreachable
    at <anonymous> (wasm://wasm/6c47e5f2:1:127)
    at file:///home/jamie/zest/test.js:23:39
```

```
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
    at <anonymous> (wasm://wasm/e1e21d7e:1:240)
    at <anonymous> (wasm://wasm/e1e21d7e:1:162)
    at file:///home/jamie/zest/test.js:23:39
```

```
%less-than(3, 7)

1

1
```

```
42 % 0

Division by zero

RuntimeError: remainder by zero
    at <anonymous> (wasm://wasm/8d761cba:1:132)
    at file:///home/jamie/zest/test.js:23:39
```

```
1 % 3

1

1
```

```
7 % 3

1

1
```

```
-1 % 3

-1

-1
```

```
-4 % 3

-1

-1
```

```
u32[1] << u32[10]

1024

1024
```

```
1 << 10

Cannot call zest.Builtin.bit-shift-left with these args: { 1, 10 }

Cannot call zest.Builtin.bit-shift-left with these args: { i64, i64 }
```

```
u32[1] << u32[32]

1

1
```

```
foo = () {
  bad grammar
}

Parse error: expected }, found zest.TokenData.name
At 2:13:
  bad grammar
             ^

Parse error: expected }, found zest.TokenData.name
At 2:13:
  bad grammar
             ^
```

```
4294967295

4294967295

4294967295
```

```
u32[4294967295]

4294967295

-1
```

```
%clz(u32[0])

32

32
```

```
%clz(u32[1])

31

31
```

```
%clz(u32[2])

30

30
```

```
%clz(u32[3])

30

30
```