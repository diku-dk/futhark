-- Defining a structure via the name of some other structure.
-- ==
-- input { 2 } output { 3 }

module M1 = { let x: i32 = 1 }
module M2 = M1

let main(x: i32): i32 = x + M2.x
