-- Memory block merging with a concat of two arrays of different sources into a
-- multidimensional array.
-- ==
-- input {  [ [1i32, 1i32, 1i32, 1i32]
--          , [1i32, 1i32, 1i32, 1i32]
--          ]
--          [3, 7]
--          [8, 9]
--       }
-- output {
--          [ [4i32, 8i32, 9i32, 10i32]
--          , [1i32, 1i32, 1i32,  1i32]
--          ]
--        }
-- structure cpu { Alloc 0 }
-- structure gpu { Alloc 0 }

let main (y: *[#n][#q]i32, a: []i32, b: []i32): *[n][q]i32 =
  let a1 = map (+1) a -- Will use the memory of z, and thereby y[0].
  let b1 = map (+1) b -- Will use the memory of z, and thereby y[0].
  let z = concat a1 b1 -- Will use the memory of y[0].
  -- There will be inserted a safety reshape here.
  let y[0] = z -- y is not allocated in this body, so there will be no
               -- allocations left after the optimisation.
  in  y
