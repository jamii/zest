Desires:
* Make costs explicit.
  * Eg don't want to accidentally copy a big value on every loop iteration.
* Make sharing values easy.
* Any expression can be turned into a function.
  * Ie no shadow checkers or unexpressible types.
* If refcounting, would be nice to be smart enough that `x = foo(x)` doesn't increment `x` - then `foo` can mutate internally without copying.

Constraints:
* In lax, has to be either enforced at runtime or be statically checkable without knowledge of the environment.
  * The current refs can be checked without knowing function types, because the calling convention captures all the needed information.
* Every value has a single concrete type, and inference predicts that type. 

I'm happy if it's not super expressive. Can always resort to copying values.