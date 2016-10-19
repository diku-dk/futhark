-- This was an issue with a simplification rule for
-- rearrange-split-rearrange chains that sometimes occur in
-- tail2futhark output.
--
-- ==
-- input { 2 3 }
-- output { [[1i32, 2i32], [4i32, 5i32]] }

fun take_arrint (l: int) (x: [][]int): [][]int =
  let (v1, _) = split (l) (x) in v1
fun reshape_int (l: int) (x: []int): []int =
  let roundUp = ((l + ((shape (x))[0] - 1)) / (shape (x))[0]) in
  let extend = reshape ((((shape (x))[0] * roundUp))) (replicate (roundUp) (x)) in
  let (v1, _) = split (l) (extend) in
  v1
entry main (x: int) (y: int): [][]int =
  let t_v1 = reshape ((x,
                       y)) (reshape_int ((x * (y * 1))) (reshape (((shape (map (fn (x: int): int =>
                                                                                (x + 1)) (iota (6))))[0] * 1)) (map (fn (x: int): int =>
                                                                                                                     (x + 1)) (iota (6))))) in
  let t_v2 = rearrange (1, 0) (t_v1) in
  let t_v3 = take_arrint (x) (t_v2) in
  let t_v4 = rearrange (1, 0) (t_v3) in
  t_v4
