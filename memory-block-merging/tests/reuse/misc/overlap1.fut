-- 'a' interferes with 'b', but not 'c'.  'c' interferes with 'b', but not 'a'.
-- Thus, the compiler should notice that it can allocate 'c' into the memory of
-- 'a'.
-- ==
-- input { [5, 2]
--       }
-- output { 41
--        }
-- structure cpu { Alloc 2 }

let main (xs: [#n]i32): i32 =
  -- START a, START b
  let (a, b) = (map (+ 1) xs, map (+ 10) xs)

  -- END a (b[0] is there to ensure this statement occurs after b)
  let a_end = b[0] + a[0]

  -- START c
  let c = map (+ 3) xs

  -- END b, END c
  let (b_end, c_end) = (b[1], c[0])

  in a_end + b_end + c_end
