Desires:
* Type-checked exhaustive error handling.
* Avoid making one-big-error the easiest path.
  * https://sled.rs/errors.html
* Want to be able to catch panics at isolation boundaries.
  * Eg functions which don't take mutable arguments?
* Panic on allocation failure. Hardly ever useful to be able to catch, hard to handle correctly, and you can use an isolation boundary when you care.
* In typed code, force annotation of throwing calls to prevent logic bugs from early return.
* Want non-local lexical returns in general, so would be nice if we could reasonably piggyback those on the same mechanism.

Unwinding vs results vs special abi:
* https://joeduffyblog.com/2015/12/19/safe-native-code/ / https://joeduffyblog.com/2016/02/07/the-error-model/
  * > A nice accident of our model was that we could have compiled it with either return codes or exceptions. Thanks to this, we actually did the experiment, to see what the impact was to our system’s size and speed. The exceptions-based system ended up being roughly 7% smaller and 4% faster on some key benchmarks.
* https://www.youtube.com/watch?v=LorcxyJ9zr4
* https://docs.rs/iex/latest/iex/
* https://smallcultfollowing.com/babysteps/blog/2024/05/02/unwind-considered-harmful/?utm_source=atom_feed
* Pass error by destination, to avoid many intermediate copies.

Unwinding isn't possible in wasm, so would want to be compatible with [wasm exceptions](https://cfallin.org/blog/2025/11/06/exceptions/) too.

## strawman

I already intend to have to have non-local returns. If we thread a `throw` function through each function, it can handle errors by returning to the site where it was defined.

```zest
result /union[ok: some-type, err: stuff-error] = {
  result = do-stuff-with(throw: (error) {
    print(%stack-trace())
    match(error,
      // Catch anything of type `stuff-error`
      (_ /stuff-error) return-to(result, [err: error]),
      // Rethrow other errors using the `throw` function passed to us.
      (_) throw(error)
  })
  [ok: result]
}
```

Pros: 

* It doesn't add any new concepts. 
* Non-local return is lexical, so we don't run into problems with dynamic scope.
* The error handler runs in the context of the throw site, so it can choose whether or not to pay the cost of generating a stack trace, rather than that being a global compiler setting.
* The `throw` function will be specialized to the type of each possible error, so we get zig-style fine-grained error inference without needing an open union type.
* When rethrowing errors, all the rethrows show up at the end of the stack trace too.

Cons:

* Have to manually pass the throw function - probably want to make this implicit.
* Functions get specialized to the type of the throw function. Probably want to wrap in a dynamic function, but then we have to add another concept to the type system to figure out what errors to specialize `throw` to.