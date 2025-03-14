Zest is a (very wip) programming language for building systems that are both [malleable](https://malleable.systems/) and [legible](https://www.scattered-thoughts.net/writing/there-are-no-strings-on-me/).

The goal is to:
* Support the interactivity and liveness of systems like emacs without giving up on civilized luxuries like static typing, early binding, jump-to-definition, load-order independence etc.
* Support the kinds of interactions I explored in research prototypes like [eve](https://witheve.com/) and [imp](https://www.scattered-thoughts.net/#imp_v3) but from the well-trodden ground of a mostly-familiar imperative language.

A good place to start reading is [docs/rationale.md](/docs/rationale.md). You can also find more notes at [scattered-thoughts.net/#zest](https://www.scattered-thoughts.net/#zest).

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