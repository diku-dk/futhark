-- ==
-- input { 1 }
-- output { 3 }

module M = import "importee-qualified"

let main(a: i32): i32 = M.whatever 1
