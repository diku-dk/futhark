-- Reuse of an unique input parameter memory block.  This requires that the
-- compiler keeps track of size equality asserts, as the two '#n's in the
-- paramaters will get different variable names by the compiler.
-- ==
-- input { [3, -3]
--         [1, 2]
--       }
-- output { [4, 5] }
-- structure cpu { Alloc 0 }

let main (x: *[#n]i32, y: [#n]i32): [n]i32 =
  let k = x[0]
  let z = map (+ k) y
  in z
