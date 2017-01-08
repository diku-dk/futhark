-- Incompatible types check
-- ==
-- error: Type s specified as t

module type T1 = { type t type s = t val a : s val f : s -> int }
module X : T1 = { type t = f32 type s = int val a : s = 3 fun f (x:s) : int = x }    -- err
fun main () : int = X.f X.a
