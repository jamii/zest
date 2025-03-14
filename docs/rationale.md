Zest is a (very wip) programming language for building systems that are both [malleable](https://malleable.systems/) and [legible](https://www.scattered-thoughts.net/writing/there-are-no-strings-on-me/).

The goal is to:
* Support the interactivity and liveness of systems like emacs without giving up on civilized luxuries like static typing, early binding, jump-to-definition, load-order independence etc.
* Support the kinds of interactions explored in research prototypes like [eve](https://witheve.com/) and [imp](https://www.scattered-thoughts.net/#imp_v3) but from the well-trodden ground of a mostly-familiar imperative language.

## wishes

Malleable:
* live reloading, as seamlessly as possible
  * cf [there are no strings on me](https://www.scattered-thoughts.net/writing/there-are-no-strings-on-me/)
* repl, eval, plugins
* introspection
  * reflect on types
  * query runtime (eg erlang)
  * query codegen (eg julia @code_native)
  * query UI eg [eve had jump-to-definition for UI elements](https://www.youtube.com/watch?v=oPyQf0rk8V4)
* repl-first tooling
  * eg [julia package manager](https://docs.julialang.org/en/v1/stdlib/Pkg/) `Pkg.add("StatsBase")`
  * eg [deno dependencies](https://docs.deno.com/examples/import_export/) `import { camelCase } from "jsr:@luca/cases@1"`
  * eg [mathematica deploy](https://www.wolfram.com/language/fast-introduction-for-math-students/en/cloud-deployment/) `CloudDeploy[Manipulate[Plot[Sin[f x], {x, 0, 2 Pi}], {f, 1, 10}]]`
* tools as libraries
  * make it easy to ship a program where the end-user can debug a problem, edit the code, live-reload to test their changes, and share a patch upstream - without any additional downloads

Legible:
* catch errors early
  * early binding
  * static type-checking
  * fail-safe apis (no undefined, no nil punning)
* take notation seriously
  * cf [the shape of data](https://www.scattered-thoughts.net/writing/the-shape-of-data/)
* more locality, less spooky-action-at-a-distance
  * no shared mutable state
  * [no life before main](https://doc.rust-lang.org/1.4.0/complement-design-faq.html#there-is-no-life-before-or-after-main-(no-static-ctors/dtors))
  * no traits/typeclasses or other global-ish state
  * no behaviour that depends on the result of type inference (eg rust `things.collect()`)
* predictable, reasonable performance
  * static dispatch
  * fields at known offsets
  * dense memory layouts
* small, simple implementation
  * possible for a single person to maintain

## decisions

Language design is a sea of interconnected decisions. Everything affects everything else. But at some point you have to pin something down. These are the decisions that seem very unlikely to change.

### imperative

This is the class of languages that we understand best, and for which it's easiest to produce reasonable, predictable performance with a simple implementation.

That means mutable references, structs, growable arrays, hashtables, for/while loops, break/continue/return etc.

### dense memory layouts

Integers/floats/structs/unions are stored inline, like in go and julia. This complicates memory management but is the easiest way to improve performance relative to languages with universal layouts like python/js/java.

Currently zest supports interior pointers too (like go and unlike julia), but this may come into conflict with other goals.

### value-oriented

All values can be compared. For any pure function, if `a == b` then `f(a) == f(b)` ie equal values are undistinguishable.

All values can be printed, (de)serialized, inspected etc. For any value, `x == deserialize(serialize(x))`. For any value that is not a function, this holds even across processes.

This simplifies the mental model (only one notation everywhere) and makes it easier to build additional tooling eg inspectors, debuggers.

The downside is that we can't define new types with custom equality rules.

### mutable xor shared

If you hold a non-mutable reference to a value, no action can change that value. If you hold a mutable reference to a value, no actions on other mutable references can change that value.

The downside is that we can't build graphs of references, or cyclic data-structures. But graphs are hard to (de)serialize anyway, and [handles](https://floooh.github.io/2018/06/17/handles-vs-pointers.html) are a reasonable workaround.

At the moment 'mutable xor shared' is implemented by an experimental type system similar to [C# refs](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/ref). If this doesn't work out, an available fallback is reference counting and copy-on-write, like swift and r.

(cf [ruminating about mutable value semantics](https://www.scattered-thoughts.net/writing/ruminating-about-mutable-value-semantics))

### structurally typed

It's very difficult to make live reloading or seamless serialization work in the presence of nominal types, because the identity of a nominal type is closely tied to both the moment of compilation and the currently running process. It's really not clear what it would even mean to send a nomimal type from one program to another.

So zest has only structural types, and those types are themselves first-class values ie can be compared and serialized.

This potentially makes the binary serialization more efficient, while still being self-describing. Eg if we want to send a value of type `list[i64]` we can first send the type itself, and then just send the length of the list followed by a direct copy of the underlying memory.

The downsides are:
* We can't use types as namespaces (eg in zig `foo.bar()` can be equivalent to `@typeOf(foo).bar(foo)` which enables oop-ish single-dispatch adhoc polymorphism).
* We can't enforce field privacy in the compiler, and instead have to rely on convention like python/julia/zig. (This does sometimes make debugging easier though).

### dynamically and statically typed

Malleability requires some amount of dynamism, but we still want static typing as much as possible. The trick is to design the dynamically-typed language so that sound static type-checking is easy (julia is very much an inspiration here):

* Every value was a first-class, concrete type.
* For a given pair of types, implicitly converting from one to the other either always succeeds or always fails. Eg implicitly converting from floats to ints can't always succeed, so it must always fail. Implicit conversions fail by throwing a runtime type error.
* When assigning to a mutable reference `a@ = b`, we implicitly convert `b` to `type-of(a)`. This includes references to struct fields, element of lists etc.
* If a function parameter is annotated with a type, we implicitly convert the argument to that type at the call-site.
* If a function return is annoted with a type, we implicitly convert all returned values to that type.

This makes it easy for a simple dataflow analysis to prove that a given program cannot throw a runtime type error. But the program behaviour never depends on the static typing - the semantics are defined entirely in terms of runtime type conversions.

There are no abstract types at all - in a given specialization of a function either every expression has a single concrete type or type-checking fails. There is no constraint solving or back-tracking.

We type-check each function __after__ specialization. The expressivity of the static type system is mostly achieved by executing (dynamically-typed) code at type-checking time, like zig/terra. There is no separate type language.

The downside is that we can't verify that a given function will type-check succesfully for every possible specialization. But this seems worth the trade for sound optional type-checking that mostly preserves the simplicity and flexibility of a dynamic language.

### interpreted and compiled

We have a similar tradeoff between runtime performance and compile latency. A common solution is to use a dynamic, interpreted language to glue together big chunks of some static, compiled language. Eg calling fortran libraries from python for numerical computing, or emacs lisp on top of a c core. 

The problem (aside from having to deal with two totally different languages) is that the static part is no longer very malleable. You can't (easily) compile and load c code from the python repl. Elisp functions can't change the behaviour of the emacs c core. Just calling between the two languages requires complex translation and marshalling.

Zest has two dialects, one dynamic/interpreted and one static/compiled, but both share the same type system and the same value representation in memory.

The idea is that in the dynamic dialect you can call `f2 = compile(f, ts)` and get a version of the function `f` that was specialized to the argument types `ts`, type-checked, optimized, and compiled. But otherwise `f2` behaves exactly the same as `f` and can be called in exactly the same way.

Similary, from inside the static dialect you can call `interpret(f, vs)` to call the dynamic/interpreted version of function `f` with the argument values `vs` and get back a value of type `any`, which you can attempt to cast to a known type.

I want to try extending this to building, packaging, and deploying code too eg a build script might look like:

```
main = import('some-code.zest').main
main/compile(struct[])/to-wasm()/write-to-file('./out.wasm')
```

Or a live-coding loop like:

```
load = () import('my-loop.zest').do-frame/compile(struct[ref[map[string, any]]])
data mut = map[string, any][]
last-frame mut = now()
do-frame mut = load()
while true {
  if file-has-changed('my-loop.zest', since: last-frame) {
    last-frame = now()
    do-frame = load()
  }
  do-frame(data@)
}
```