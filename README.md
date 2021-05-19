# lang 2

continued experiments with language dev

This repository currently has an implementation of a System Fω type
checker and inference engine. To test the type system, without inference,
there is a REPL in `bin/lambda.rs`.

# notes

## a type level array implementation:
```
// T is the type of element and N is a church number representing the size
// of the array
type Array T N = struct { val : T, len : Ite (IsZ N) Nil (Array T (Pred N)) }
```

This should work like a normal array, the reduced type will be something like:

```
struct {
	val : T,
	next : struct {
		val : T
		next : struct {
			val : T,
			..
		}
	}
}
```

Which ultimately gets laid out as N sequential Ts. Accesses would use
pointer arithmetic and bounds checking would use a polymorphic function
from church numbers to the native integers `N -> Int`:

```
type Church0 F X = X
type Church1 F X = F X
type Church2 F X = F F X


type ChurchToIntBase = Nil
fn toInt<S = ChurchToIntBase>() -> Int {
	return 0
}

type ChurchToIntSucc N = struct { inner : N }
fn toInt<S = ChurchToIntSucc>() -> Int {
	return 1 + toInt<S.inner>()
}
```

