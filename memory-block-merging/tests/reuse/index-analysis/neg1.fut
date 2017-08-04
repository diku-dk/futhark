-- A negative test.
-- ==
-- input { [5, 7]
--       }
-- output { [14, 12]
--        }
-- structure cpu { Alloc 2 }

-- Similar to the function in neg0.fut, except this will be a kernel when
-- possible, and not always a loop, so we can test the optimisation in the
-- compiler's GPU pipeline.
let interfering_map (k: i32) (t: [#n]i32): [n]i32 =
  map (\i -> t[n - i - 1] + k) [0..<n]

let main (ns: [#n]i32): [n]i32 =
  let xs = map (+ 1) ns
  let k0 = xs[0]
  let ys = interfering_map k0 xs
  in ys
