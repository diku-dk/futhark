-- ==
-- input { 1 }
-- output { 3 }

module M = import "importee-qualified"

fun main(a: i32): i32 = M.whatever 1
