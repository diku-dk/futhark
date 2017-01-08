module type T1 = { type t type s = t val a : s val f : s -> int }
module X : T1 = { type t = int type s = int val a : s = 3 fun f (x:s) : int = x }    -- ok
fun main () : int = X.f X.a
