-- Opaque signature ascription must hide equality of type.
-- ==
-- error: type

module type S = { type t val a : t val f : t -> int }
module B : S = { type t = int val a:t = 3 fun f (a:t):t = a }
module C : S = B
fun main() : int = C.f B.a
