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