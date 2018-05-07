-- This was an issue with a simplification rule for
-- rearrange-split-rearrange chains that sometimes occur in
-- tail2futhark output.
--
-- ==
-- input { 2 3 }
-- output { [[1i32, 2i32], [4i32, 5i32]] }

let take_arrint (l: i32) (x: [][]i32): [][]i32 =
  let (v1, _) = split (l) (x) in v1
let reshape_int (l: i32) (x: []i32): []i32 =
  let roundUp = ((l + (length x - 1)) / length x) in
  let extend = flatten (replicate (roundUp) (x)) in
  let (v1, _) = split (l) (extend) in
  v1
entry main (x: i32) (y: i32): [][]i32 =
  let t_v1 = unflatten x y (reshape_int ((x * (y * 1))) (map (\(x: i32): i32 ->
                                                                                                                     (x + 1)) (iota (6)))) in
  let t_v2 = rearrange (1, 0) (t_v1) in
  let t_v3 = take_arrint (x) (t_v2) in
  let t_v4 = rearrange (1, 0) (t_v3) in
  t_v4
