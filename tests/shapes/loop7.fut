-- Infer correctly that the loop parameter 'ys' has a variant size.
-- ==
-- input { [0,1] } output { 2 [0i32] }

let first_nonempty f xs =
  loop (i, ys) = (0, [] : []i32) while null ys && i < length xs do
  let i' = i+1
  let ys' = f xs[i]
  in (i', ys')

let main [n] (xs: [n]i32) =
  first_nonempty iota xs
