-- Reuse the unique input parameter memory while still having to perform index
-- analysis.
-- ==
-- input { [5, 7]
--       }
-- output { [6, 8]
--        }
-- structure cpu { Alloc 0 }
-- structure gpu { Alloc 0 }

let main (ns: *[#n]i32): [n]i32 =
  let xs = map (+ 1) ns
  in xs
