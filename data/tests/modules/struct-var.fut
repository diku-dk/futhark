-- Defining a structure via the name of some other structure.
-- ==
-- input { 2 } output { 3 }

module M1 { val x: int = 1 }
module M2 = M1

fun main(x: int): int = x + M2.x
