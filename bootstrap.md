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

TODO infer: dir.ExprData{ .string = { 120 } }
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

TODO infer: dir.ExprData{ .string = { 120 } }
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

Expected an object, found: i32
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

Expected struct['x': i32, 'y': i32], found i32

Expected struct['x': i32, 'y': i32], found i32
```

```
a mut = [x: 1, y: 2]
a.x@ = a
a.x.x

Expected i32, found struct['x': i32, 'y': i32]

Expected i32, found struct['x': i32, 'y': i32]
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

Expected a value containing no mutable references, found: ref[i32][1]

Expected a value containing no mutable references, found: ref[i32]
```

```
a mut = 1
[a@]

Expected a value containing no mutable references, found: [ref[i32][1]]

Expected a value containing no mutable references, found: struct[ref[i32]]
```


```
a mut = 1
b = a@

Expected a value containing no mutable references, found: ref[i32][1]

Expected a value containing no mutable references, found: ref[i32]
```

```
a mut = 1
b = [a@]

Expected a value containing no mutable references, found: [ref[i32][1]]

Expected a value containing no mutable references, found: struct[ref[i32]]
```

```
f = (x) x
a mut = 1
f(a@)

Expected a value containing no mutable references, found: ref[i32][1]

Expected a value containing no mutable references, found: ref[i32]
```


```
f = (x mut) x
a mut = 1
f(a)

Expected a mutable reference, found: 1

Expected a mutable reference, found: i32
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

Cannot call zest.Builtin.add with these args: { i32, struct[i32] }
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

Expected a value containing no mutable references, found: fun[id: 1, closure: struct['a': ref[i32]]]['a': ref[i32][42]]

Expected a value containing no mutable references, found: fun[id: 1, closure: struct['a': ref[i32]]]
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

Expected a value containing no mutable references, found: fun[id: 0, closure: struct['a': ref[i32]]]['a': ref[i32][42]]

Expected a value containing no mutable references, found: fun[id: 0, closure: struct['a': ref[i32]]]
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
i32
string
repr
42

42

TODO infer: dir.ExprData{ .repr_i32 = void }
```

```
i32 = 1

Name already bound: i32

Name already bound: i32
```

```
[i32: 101].i32

101

101
```

```
[{i32}: 101].{i32}

101

101
```

```
f = (i32) 42
f(0)

Name already bound: i32

Name already bound: i32
```

```
struct
only
42

42

TODO infer: dir.ExprData{ .repr_kind_struct = void }
```

```
i32[42]

42

42
```

```
a = struct[x: i32, y: i32][x: 42, y: 101]
a.y

101

101
```

```
a = struct[x: i32, y: i32][x: 42, y: []]
a.y

Expected struct['x': i32, 'y': i32], found struct['x': i32, 'y': struct[]]

Expected struct['x': i32, 'y': i32], found struct['x': i32, 'y': struct[]]
```

```
a = struct[x: i32, y: i32][x: 42, z: 101]
a.y

Expected struct['x': i32, 'y': i32], found struct['x': i32, 'z': i32]

Expected struct['x': i32, 'y': i32], found struct['x': i32, 'z': i32]
```

```
a = struct[x: i32, y: i32][x: 42, y: 101, z: 0]
a.y

Expected struct['x': i32, 'y': i32], found struct['x': i32, 'y': i32, 'z': i32]

Expected struct['x': i32, 'y': i32], found struct['x': i32, 'y': i32, 'z': i32]
```

```
a mut = [42]
// Pointless copy here because make expr blocks dest
b = struct[struct[i32]][a]
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

0

128
```

```
%memory-grow(3)

0

128
```

```
%memory-grow(3)
%memory-grow(4)

3

131
```

```
%memory-grow(3)
%memory-grow(4)
%memory-size()

7

135
```