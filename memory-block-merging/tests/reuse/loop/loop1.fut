-- Simple reuse inside a loop.  The reuser should be able to set xs2 to use the
-- same memory as xs1.
-- ==
-- input { [3, 4, 5]
--       }
-- output { [31, 32, 33]
--        }
-- structure cpu { Alloc 2 }
-- structure gpu { Alloc 2 }

let main [n] (xs0: [n]i32): [n]i32 =
  loop xs = xs0 for _i < n do
    let xs1 = map (+ 1) xs
    let xs2 = map (+ xs1[0]) xs
    in xs2
