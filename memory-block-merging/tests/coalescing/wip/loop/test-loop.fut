-- Test1 Memory-Block Merging
--
-- Same problem as in loop-existential.fut.
-- ==
-- input { [0, 3, 5, 7, 9, 11] }
-- output { [0, 384, 640, 896, 1152, 1408, 14, 18, 22] }

-- structure cpu { Alloc 5 }
-- structure gpu { Alloc 5 }

-- We can remove two allocations (from 7 to 5) with coalescing.  More
-- allocations can be removed by the register allocation reuse algorithm.

let main [n] (x: [n]i32): []i32 =
  let y = map (*2) x in
  let y' = reshape (2, n/2) y
  let a = loop a = y for _i < n do
      let b = map (* 2) a
      let c = map (+ b[0]) b
      let d = map (+ c[0]) c
      let e = map (+ d[0]) d
      in  e -- `e` can be coalesced into the inserted double buffer memory.

  -- `a` can be coalesced into `w`.  `y'[1]` cannot be coalesced into `w`, since
  -- `a` now uses the memory of `w`.  However, they *do* use non-overlapping
  -- locations in the memory of w, so in this case a coalescing would seemingly
  -- be okay (though that would require more compiler work).
  let w = concat a y'[1]
  in w
