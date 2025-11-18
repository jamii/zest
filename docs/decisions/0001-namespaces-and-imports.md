# namespaces and imports

The core problems to be solved:
* We want to be able to split code across multiple files and packages.
* We want to be able to combine files and packages without having to worry about name collisions.

The basics feel settled in my mind:

A namespace consists of a map from identifiers to values. The values are specified by zest expressions, which are evaluated in a pure context (to allow caching and parallel loading). Declarations can refer to each other in any order, allowing for mutually recursive functions.

```
is-even = (n) {
  if {n == 0} 
    'true'
  else 
     is-odd(n - 1)
}

is-odd = (n) {
  if {n == 0}
    'false'
  else
    is-odd(n - 1)
}

evens = range(1, 10)/filter(is-even)

print-is-even = () {
  print(is-even(read-int()))
}
```

The cli allows importing namespaces and running functions:

```
zest test.zig print-is-even
```

We can break code up into multiple files by importing one file namespace into another:

```
odds-and-evens = %import('./odds-and-evens.zest')

main = () print(odds-and-evens.is-even(42))
```

We can later extend `%import` to allow deno-style library imports:

```
http = %import('https://scattered-thoughts.net/zest-pkgs/http', hash: '...')
```

(The hash argument can be optional, and we can have a cli tool that inserts them for you.)

---

The hard questions all tie into one core question: what does `%import` return? What is a namespace?

After mulling this over for a long time I see two compelling local minima.

## STRUCTS: namespaces are compile-time-known structs, evaluated strictly

In this version, `%import` returns a compile-time-known struct:

```
> print(%import('./odds-and-evens.zest'))

[
  is-even: fn[0][],
  is-odd: fn[1][],
  evens: [2, 4, 6, 8, 10],
  print-is-even: fn[2][],
]
```

The entire namespace is evaluated strictly at import time. The declarations are topo-sorted and evaluated in some compatible order. If there is a cyclic dependency between declaration evaluation, `%import` returns an error. 

Mutually recursive functions are still allowed within a namespace, but since `%import` is evaluated strictly it is not possible to have mutually recursive functions across namespaces. Namespace imports must form a DAG.

Since the result is just a struct, it can be manipulated using all the basic features of the language. Declarations are accessed using `.` or `get`. We can easily make new namespaces, either directly or using metaprogramming.

## HANDLES: namespaces are handles, evaluated lazily

In this version, `%import` returns a nominally-typed handle, similar to a function.

```
> print(%import('./odds-and-evens.zest'))

namespace[0][]
```

At import-time we just parse the file and store a map from identifier to expression. 

Declarations are accessed by a new operator, let's say `::` for now. When accessing a declaration we:

* Check if the declaration is marked as 'evaluated'. If so we return the cached value.
* Check if the declaration is marked as 'evaluating`. If so we return a circular dependency error.
* Mark the declaration as 'evaluating'. 
* Evaluate the expression.
* Make the declaration as 'evaluated', cache the value and return it.

(I introduce a new operator rather than overloading `.` because accessing declarations can cause arbitrary evaluation, may require loading files or downloading packages, and can return arbitrary errors. That's a lot to pack into an operator that is otherwise just a pointer offset (compiled) or a hashtable lookup (interpreted). Also the current semantics of `.` are that it's a field lookup on objects, but namespace handles can't be objects in this sense because printing a namespace doesn't print their declarations, so it would pollute the data model.)

Mututally recursive declarations are possible across namespaces, so long as their evaluation does not produce an error.

When type-checking or compiling a function, we would have to force all reachable declarations.

Since a namespace handle is a new kind of value, and the rest of the language doesn't have access to lazy evaluation, it will be tricky to provide any support for building namespaces with metaprogramming. But we could still allow creating namespaces with fixed declaration names, like in zig:

```
lsm = (io) namespace{
  insert = ...
  remove = ...
}

lsm-test = lsm(io-test)
```

This would behave the same way as function closures:

```
> print(lsm-test)

module[1][io: module[0][]]
```

I've used fresh integer handles in the examples, but we could make other choices for namespace identity and this gives us some flexibility in deciding how partial live-code reloading might work. Using a fresh id gives us julia-style reloading, using the file path would give us erlang-style reloading, using a content-addressed hash would give us unison-style reloading, etc.

---

Either option has a lot of downstream consequences.

## runtime errors

I haven't decided yet how error handling will work in general.

Any `::` operator can return any error. This makes it very difficult to require that all potential errors are annotated in the dynamic dialect.

This doesn't affect the static dialect though, since all the `::` will be forced during type-checking, so we can still check exhaustive error-handling for all runtime errors.

## development errors

Eg writing code that calls a function that doesn't exist.

STRUCT will immediately surface any errors anywhere in the project. HANDLE will only surface errors that are reached by code we are currently executing (or are reachable, if we type-check/compile code). Neither feels strictly better. Laziness can be very nice during development where it allows trying out ideas in a small area of code first having to refactor everything that depends on that code. But it also makes it easy to miss mistakes (eg in zig a common mistake is to write a test that is not forced from the top-level and so doesn't actually get run).

We could improve this somewhat in HANDLE with a little meta-programming:

```
force-all = (namespace) {
  namespace/declarations()/each((decl) {
    _ = namespace::{decl}
  })
  namespace/imports()/each(force-all)
}
```

## printing

In STRUCT it's easy to see what a namespace contains by printing it. But if you print `std` then you're going to get an unreadable mess that obliterates your terminal history.

In HANDLE you have to do more work to print things, and be careful to check for errors when forcing declarations:

```
print-declarations = (namespace) {
  namespace/declarations()/each((decl) {
    value = namespace::{decl}/catch((err) err)
    print(f'{decl} = {value}\n')
  })
}
```

## dependency management

It's useful to allow importing multiple versions of the same library. Not just when resolving dependency trees, but also for testing:

```
fuzz(%import('foo/v1'), %import('foo/v2'))
```

For STRUCT this is trivial. Importing just produces a value.

For HANDLE whether or not this is possible depends on how we assign identity to namespaces. If we use fresh integer handles or content-addressed identity this is fine. If we use paths we might have issues, unless the version is somehow included in the path.

## ide support

In both cases, any ide support is probably going to involve evaluating declarations in some resource-bounded sandbox in an error-tolerant mode. 

A complication for STRUCT is that there isn't an obvious place to limit errors. If we depend on a namespace that has an error in one declaration, we want ide support to still work for the other declarations. But what should our sandboxed evaluation return for the import? If we drop the erroneous declaration then it changes the type of the struct. If we include the error, then it looks like a regular value.

## load times

Library loading time matters a lot for scripts. They can be surprisingly bad in dynamic languages eg `python3 -X importtime -m http.server` reports a loading time of 83ms cold or 59ms warm. [Impatient](https://github.com/lewis6991/impatient.nvim) reports that their neovim config takes 34ms to load, and caching only reduces this to 7ms. (They also report another 54ms spent searching import paths, which is a misfeature we absolutely should not offer).

On my machine, deleting the zig cache adds ~4-5s to the build time for hello world. I _believe_ this time is spent parsing the stdlib.

With STRUCT we would have to parse and evaluate the entire stdlib on import. Caching would be trivial but we would still have to deserialize the cache for the entire stdlib into memory on each script run. Cold load times are important for CI, so caching is not a complete panacea. Splitting the stdlib into smaller packages would help, but we'll face the same problem for big 3rd-party packages. Having to split packages is also annoying - it leads to big piles of import declarations.

With HANDLE we would only have to parse and evaluate parts of the stdlib that we actually depend on. Caching becomes more complicated, since we have to cache individual declarations as they are forced rather than caching them all at import-time, and those declarations might also form cycles which we have to be careful about during invalidation. Lazy imports would make it possible to import mega batteries-included packages (like the old [haskell platform](https://web.archive.org/web/20211129171626/https://www.haskell.org/platform/contents.html) or [ocaml batteries](https://ocaml-batteries-team.github.io/batteries-included/hdoc2/)) while only paying the cost of the packages that we actually use.

A downside of HANDLE is that import times now get mixed up unpredictably with the rest of our code. But when we type-check or compile functions we force all reachable declarations, and the expected usage is that we mostly run type-checked code and only use dynamic typing for compile-time meta-programming and malleable-programming glue. So a script might look like:

```
#!/usr/bin/env zest run

batteries = %import('...')

do-stuff = () {
   // use something from batteries
   ...
}

// On first run, force all reachable declarations and cache compiled code.
// On subsequent runs, just run code from the cache.
main = %compile(do-stuff, struct[], cache: 'global')
```

## reloading

With both options we can do whole-program reloading with incremental compilation.

With HANDLE, we also have the option to have reasonable semantics for reloading individual namespaces. This is harder for STRUCT because the result of importing is a regular value that can be copied and escape.

Supporting some kind of repl is similar. With HANDLE we can treat the repl as a namespace which we are repeatedly reloading with new declarations. With STRUCT we may need the repl to have slightly different semantics to regular code.

## tree-shaking

When compiling code and emitting a standalone executable we want to be able to keep the executable as small as possible. For functions this is easy - we already get a list of specializations out of type-checking. But for constants it's trickier.

During compilation we can require that for all expressions `namespace::{decl}`, the value `decl` is compile-time known

For HANDLE this is sufficient. Type-checking will give us a list of reachable constants.

For STRUCT it's a little tricker, because we might still have references to the namespace itself. If we ever write something like `foo(%import('std'))` and the parameter of foo isn't declared as compile-time known, then the entire namepspace escapes and we have to include the struct itself, all constants, and all upstream namespaces into the executable. This doesn't seem like it would be a common mistake though.

## destructuring

In STRUCT we can use destructuring to write:

```
[:is-even, :is-odd, ...] = %import('./odds-and-evens.zest')
```

Because destructuring desugars to:

```
anon = %import('./odds-and-evens.zest')
asssert(is-object(anon))
is-even = anon.is-even
is-odd = anon.is-odd
```

In HANDLE there are some obstacles.

First, we didn't overload `.` so we'd have to extend destructuring to handle namespace declarations.

```
[::is-even, ::is-odd, ...] = %import('./odds-and-evens.zest')

// desugars to

anon = %import('./odds-and-evens.zest')
assert(is-namespace(anon))
is-even = anon::is-even
is-odd = anon::is-odd
```

Second, we'd have to figure out how lazy evaluation interacts with the assertions produced by destructuring. Perhaps they have to be wrapped around the initial value?

```
anon = %import('./odds-and-evens.zest')/with-assert(is-namespace)
is-even = anon::is-even
is-odd = anon::is-odd
```

---

## Decision

HANDLE requires adding some new concepts to the language - namespace types, lazy evaluation, `::` - whereas STRUCT is able to do everything with existing concepts.

HANDLE adds a big source of unpredictability via laziness. But it also offers better options for ide support, minimizing load times, development experience, live reloading / repls, and avoids the make-work of splitting code into granular packages (cf rust workspaces).

I think HANDLE adds enough to pay for the extra features.