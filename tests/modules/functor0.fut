-- Basic test of simple functor.
-- ==
-- input { 5 } output { 8 }

module type SIG { val x: i32 }
module S0: SIG { val x: i32 = 2 }
module F0(P: SIG) { val x: i32 = P.x + 1 }
module S1 = F0(S0)

fun main(x: i32): i32 = x + S1.x
