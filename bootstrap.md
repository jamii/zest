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

Not an object: 1

Not an object: i32
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
a = mut 1
a

1

1
```

```
a = mut 1
a@ = 2
a

2

2
```

```
a = mut [x: 1, y: 2]
a.x@ = 3
a.x

3

3
```

```
a = mut [x: 1, y: 2]
a.x@ = 3
a.y

2

2
```

```
a = mut [x: 1, y: 2]
a.y@ = 3
a.x

1

1
```

```
a = mut [x: 1, y: 2, z: 3]
a.y@ = 3
a.y

3

3
```

```
a = mut [x: 1, y: 2, z: 3]
a@ = [x: a.y, y: a.x, z: a.z]
a.x

2

2
```

```
a = mut [x: 1, y: 2, z: 3]
a@ = [x: a.y, y: a.x, z: a.z]
a.y

1

1
```

```
a = mut [x: 1, y: 2, z: 3]
a@ = [x: a.y, y: a.x, z: a.z]
a.z

3

3
```


```
a = mut [x: 1, y: 2]
a@ = 3
a

Expected struct['x': i32, 'y': i32], found i32

Expected struct['x': i32, 'y': i32], found i32
```

```
a = mut [x: 1, y: 2]
a.x@ = a
a.x.x

Expected i32, found struct['x': i32, 'y': i32]

Expected i32, found struct['x': i32, 'y': i32]
```

```
a = mut 1
b = a
a@ = 2
b

1

1
```

```
a = mut [1]
b = a
a@ = [2]
b.0

1

1
```