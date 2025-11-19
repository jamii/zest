Currently some parts of the syntax mark values as needing to be staged. Problems:
* Once we add list/map, we'll want to allow struct literals with non-staged keys eg `map[string, string][{a}: b]`.
* The split syntax for function calls vs type conversion is a mess. Would be nicer to allow calling types like functions.

Perhaps we want a dedicated syntax for forcing staging, and use it for eg struct keys? Maybe something like `.foo == .{'foo'}`. Then lends the meaning to `bar.foo` as `bar(.{'foo'})` nicely.

---

Currently staged-ness is represented by lifting to a type. Problems:
* Integer/string literals need to be constant so that we can allow eg `u8[42]` but not `u8[3000]` or `u8[x]`. But if we lift them to a different type then the default in lax code is weird?

We could represent staged-ness in infer with walues instead, and propagate it through function calls with an annotation similar to `ref`.

As a bonus we could then do:

```
// k has type string, function only gets one specialization
some-struct/each((k, _) ...)

// k has constant value, function specializes per key
some-struct/each((k const, _) ...)
```

---

Would be nice to have type conversions just be function application, but then we don't know that they need to be staged until after type inference, at which point it's awkward to go back and delete all their tir.