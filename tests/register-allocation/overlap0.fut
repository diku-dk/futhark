-- 'a' interferes with 'b', so they cannot use the same memory.
-- ==
-- input { [3, 1]
--       }
-- output { 17
--        }

-- structure cpu { Alloc 2 }

let main (xs: [#n]i32): i32 =
  -- START a, START b
  let (a, b) = (map (+ 1) xs, map (+ 10) xs)

  -- END a, END b
  let (a_end, b_end) = (a[0], b[0])

  in a_end + b_end
