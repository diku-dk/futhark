-- Pathological case.
-- ==
-- input {
--   [0] 0
--   [1, 5] 1
--   [-4, 10, 1] 2
--   [13, 10, 12, 9] 3
-- }
-- output { 19 }
-- structure cpu { Alloc 1 }
-- structure gpu { Alloc 1 }

let main (
  a: []i32, ai: i32,
  b: []i32, bi: i32,
  c: []i32, ci: i32,
  d: []i32, di: i32
): i32 =
  let a1 = map (+ 1) a
  let av = a1[ai]

  let b1 = map (+ 1) b
  let bv = b1[bi]

  let c1 = map (+ 1) c
  let cv = c1[ci]

  let d1 = map (+ 1) d
  let dv = d1[di]

  in av + bv + cv + dv
