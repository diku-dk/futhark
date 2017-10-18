-- A negative test.
-- ==
-- input { [5, 7]
--       }
-- output { [0, 12]
--        }
-- structure cpu { Alloc 2 }
-- structure gpu { Alloc 2 }

-- Kind of map, but do it with offset write indices.  The compiler should notice
-- this.
let interfering_map [n] (k: i32) (t: [n]i32): [n]i32 =
  loop u = replicate n 0 for i < n - 1 do
    let u[i + 1] = t[i] + k
    in u

let main [n] (ns: [n]i32): [n]i32 =
  let xs = map (+ 1) ns
  let k0 = xs[0]
  let ys = interfering_map k0 xs
  in ys
