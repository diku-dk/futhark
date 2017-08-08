-- A test for arrays with different dimensions but the same underlying byte
-- sizes.
--
-- Since this contains a nested map, and since we don't perform coalescing or
-- in-place lowering in the memory reuse tests, the CPU pipeline will have an
-- extra alloc for the inner loop.
-- ==
-- input { [[1, 10]]
--       }
-- output { [[2], [2]]
--        }
-- structure cpu { Alloc 2 }
-- structure gpu { Alloc 1 }

let main (xss: [#m][#n]i32): [n][m]i32 =
  -- Create a new array.
  let yss = map (\xs -> map (+ 1) xs) xss

  -- Create another new array.  This has different dimensions than yss, but the
  -- same byte size, so it should be able to use the same memory as yss.
  let zss = replicate n (replicate m yss[0, 0])
  in zss
