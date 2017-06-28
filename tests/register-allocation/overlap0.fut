-- 'a' interferes with 'b', but not 'c'.  'c' interferes with 'b', but not 'a'.
-- Thus, the compiler should notice that it can allocate 'c' into the memory of
-- 'a'.
-- ==
-- input { [1, 2]
--       }
-- output { [3, 3]
--        14
--        }

-- structure cpu { Alloc 2 }

let main (xs: [#n]i32): ([n]i32, i32) =
  let a = map (+ 1) xs -- START a
  let b = map (+ 2) xs -- START b
  let a_end = (map (+) b a)[0] -- END   a
  let c = replicate n 3         -- START c
  let b_end = (map (+) c b)[0] + a_end -- END   b
  let c_end = c[0] + b_end             -- END   c
  in (c, c_end)
