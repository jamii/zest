Zest is a (very wip) programming language for building systems that are both [malleable](https://malleable.systems/) and [legible](https://www.scattered-thoughts.net/writing/there-are-no-strings-on-me/).

The goal is to:
* Support the interactivity and liveness of systems like emacs without giving up on civilized luxuries like static typing, early binding, jump-to-definition, load-order independence etc.
* Support the kinds of interactions I explored in research prototypes like [eve](https://witheve.com/) and [imp](https://www.scattered-thoughts.net/#imp_v3) but from the well-trodden ground of a mostly-familiar imperative language.

A good place to start reading is [docs/rationale.md](/docs/rationale.md). You can also find more notes at [scattered-thoughts.net/#zest](https://www.scattered-thoughts.net/#zest).

## status

* Basic control flow, arithmetic, comparisons, functions etc work.
  * Break/continue/return are missing
  * Mutually recursive functions are not supported yet (the interaction with staging is tricky, see #1).
* There are 2nd-class mutable references, but dynamic/static prevention of aliasing is incomplete.
* The type system, specialization, and compile-time evaluation more or less work, but the ergonomics could be improved.
* Code can either by interpreted or compiled, but there is no support yet for mixing both within a single program.
* Interpreted code leaks memory. Compiled code stack-allocates everything (which is safe because type-system prevents references escaping).
* There is no heap allocation and no memory management yet. The allocator exists in [/lib/runtime.zest](/lib/runtime.zest) but hooking it up to the language requires solving #1 first.
* The only error-handling available is `panic`.

## docs and tests

The [docs](/docs) contain embedded tests that look like this:

```
// code
1 + 1

// result
2
```

When the lax and strict dialects produce different results, there will be two results in the test:

```
// code
1 + 'foo'

// lax result
Cannot call zest.Builtin.add with these args: { 1, 'foo' }

// strict result
Cannot call zest.Builtin.add with these args: { i64, string }
```

The strict dialect doesn't currently have a way to print values from inside the wasm sandbox, so any test that returns a non-integer value will only print `undefined` in the strict dialect.

```
'foo'

'foo'

undefined
```

The implementation is in flux. Generally, the docs text will describe the intended behaviour, but the tests will show the current behaviour. The tests can be automatically updated with `zig run lib/test.zig -- --rewrite docs/*.md`.