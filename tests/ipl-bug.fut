-- A contrived program that makes in-place lowering fail.
--
-- Think about enabling this test once we have replaced in-place lowering with
-- memory block merging.
-- ==
-- tags { disable }

let main [n] (xs: *[n][n]i32, ys0: [n]i32, i: i32): ([n][n]i32, i32) =
  let ys = map (+ 1) ys0
  let zs = map (+ 1) xs[i]
  let xs[i] = ys
  in (xs, zs[i])
