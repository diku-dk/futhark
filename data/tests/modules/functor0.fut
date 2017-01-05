-- Basic test of simple functor.
-- ==
-- input { 5 } output { 8 }

module type SIG { val x: int }
module S0: SIG { val x: int = 2 }
module F0(P: SIG) { val x: int = P.x + 1 }
module S1 = F0(S0)

fun main(x: int): int = x + S1.x
