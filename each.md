```
%each(1, (k, v) %print(k))

error

error
```

```
// TODO Add a char type
%each('hello world', (k, v) %print(k))

error

error
```

```
%each([a: 1, b: 2], (k, v) {
  %print(%from-only(k))
  %print('\n')
  %print(v)
  %print('\n')
})

a
1
b
2
[]

a
1
b
2
[]
```

```
%each(union[a: i64, b: i64][[a: 1]], (k, v) {
  %print(%from-only(k))
  %print('\n')
  %print(v)
  %print('\n')
})

a
1
[]

a
1
[]
```



```
%each(union[a: i64, b: i64][[b: 2]], (k, v) {
  %print(%from-only(k))
  %print('\n')
  %print(v)
  %print('\n')
})

b
2
[]

b
2
[]
```