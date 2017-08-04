-- This program is a demonstration of how the linear scan register
-- allocation-inspired algorithm of memory reuse does not produce an optimal
-- result.  The optimisation can reduce the number of allocations from 4 to 3,
-- while an optimal analysis can reduce it to 2 allocations.
-- ==
-- structure cpu { Alloc 3 }

-- Interference table:
--   xs0 interferes with nothing
--   xs1 interferes with nothing
--   xs2 interferes with xs1, xs3
--   xs3 interferes with xs0, xs2

-- The implemented top-down traversal (one merging):
--   xs0 cannot use anything
--   xs1 uses xs0; xs0 and xs1 now interfere
--   xs2 cannot use anything
--   xs3 cannot use anything

-- The optimal traversal (two mergings):
--   xs2 uses xs0
--   xs3 uses xs1
--   xs0 cannot use anything
--   xs1 cannot use anything

let interfering_map (k: i32) (t: [#n]i32): [n]i32 =
  loop u = replicate n 0 for i < n - 1 do
    let u[i + 1] = t[i] + k
    in u

let interfering_map2 (k: i32) (t0: [#n]i32) (t1: [#n]i32): [n]i32 =
  loop u = replicate n 0 for i < n - 1 do
    let u[i + 1] = t0[i] + t1[i] + k
    in u

let main (ns: [#n]i32): [n]i32 =
  let k0 = 1
  let xs0 = map (+ k0) ns

  let k1 = xs0[0]
  let xs1 = map (+ k1) ns

  let k2 = xs1[0]
  let xs2 = interfering_map k2 xs1

  let k3 = xs2[0]
  let xs3 = interfering_map2 k3 xs0 xs2

  in xs3
