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

TODO generate
```

```
a = (x, y) [:x, :y]
a(7, 11).y

11

TODO generate
```

```
a = 1
b = () a
b()

1

TODO generate
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

TODO generate
```

```
a = ([:x]) x
a([x: 1])

1

TODO generate
```

```
a = ([:x]) x
a(1)

Not an object: 1

Not an object: i32
```