-- Test1 Memory-Block Merging
-- ==
-- input { [0, 3, 5, 7, 9, 11] }
-- output { [0, 384, 640, 896, 1152, 1408, 14, 18, 22] }
-- structure cpu { Alloc 4 }

-- This is the same test as the one in coalescing/loop/ (see that for coalescing
-- comments), but here we run it with both coalescing and reuse.
--
-- The reuse pass currently adds one more memory block merging (from 5 to 4).
-- More are possible if we analyse the access patterns in the map bodies; read
-- more in coalescing/wip/loop/loop-existential-loop-error.fut.

let main (x: [#n]i32): []i32 =
  let y = map (*2) x in
  let y' = reshape (2, n/2) y
  let a = loop (a = y) for _i < n do
      let b = map (* 2) a
      let c = map (+ b[0]) b
      let d = map (+ c[0]) c -- Can reuse the memory of b.
      let e = map (+ d[0]) d
      in  e

  let w = concat a y'[1]
  in w
