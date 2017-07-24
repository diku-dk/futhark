-- A test for arrays with different dimensions but the same underlying byte
-- sizes.
--
-- FIXME: This does not work right now because of the primitive size comparison.
-- We need to try something with converting to 'PrimExp's and comparing those
-- liberally.
-- ==
-- input { [[1, 10]]
--       }
-- output { [[2], [2]]
--        }

-- structure cpu { Alloc 1 }

let main (xss: [#m][#n]i32): [n][m]i32 =
  -- Create a new array.
  let yss = map (\xs -> map (+ 1) xs) xss

  -- Create another new array.  This has different dimensions than yss, but the
  -- same byte size, so it should be able to use the same memory as yss.
  let zss = replicate n (replicate m yss[0, 0])
  in zss
