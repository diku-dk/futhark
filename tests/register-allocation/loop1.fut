-- Register allocation should be able to set xs' to use the same memory as xs.
-- ==
-- input { [3, 4, 5]
--       }
-- output { [6, 7, 8]
--        }

-- structure cpu { Alloc 1 }

let main (xs0: [#n]i32): [n]i32 =
  loop (xs = xs0) for _i < n do
    let xs' = map (+ 1) xs
    in xs'
