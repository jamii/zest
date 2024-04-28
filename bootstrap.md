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
a = [x: 1, y: 2]
a.x

1

TODO lower
```

```
a = [x: 1, y: 2]
a.y

2

TODO lower
```

```
a = [[x: 1, y: 2], z: 3]
a.0.x

1

TODO lower
```

```
a = [[x: 1, y: 2], z: 3]
a.0.y

2

TODO lower
```

```
a = [[x: 1, y: 2], z: 3]
a.z

3

TODO lower
```

```
// TODO Should all constants be stageable?
k = 'x'
a = [{k}: 1]
a.x

1

Cannot unstage value: repr.Repr{ .string = void }
```

```
k = () 'x'
a = [{k()}: 1]
a.x

1

TODO lower
```

```
k = 'x'
k2 = () k
a = [{k2()}: 1]
a.x

1

Cannot unstage value: repr.Repr{ .fun = repr.ReprFun{ .fun = dir.Fun{ .id = 0 }, .closure = repr.ReprStruct{ .keys = { ... }, .reprs = { ... } } } }
```

```
a = () 1
b = 2
a()

1

TODO lower
```

```
a = (x) x
b = 2
a(1)

1

TODO lower
```

```
a = (x, y) x
a(7,11)

7

TODO lower
```

```
a = (x, y) y
a(7,11)

11

TODO lower
```

```
a = (:x, :y) y
a(y: 7, x: 11)

7

TODO lower
```

```
a = (b) b.y
a([x: 11, y: 7])

7

TODO lower
```

```
a = (x, y) [:x, :y]
a(7, 11).x

7

TODO lower
```

```
a = (x, y) [:x, :y]
a(7, 11).y

11

TODO lower
```

```
a = 1
b = () a
b()

1

TODO lower
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

TODO lower
```

```
a = ([:x]) x
a([x: 1])

1

TODO lower
```

```
a = ([:x]) x
a(1)

Not an object: 1

Not an object: repr.Repr{ .i32 = void }
```