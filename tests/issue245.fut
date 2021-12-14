-- This was an issue with a simplification rule for
-- rearrange-split-rearrange chains that sometimes occur in
-- tail2futhark output.
--
-- ==
-- input { 2i64 3i64 }
-- output { [[1i32, 2i32], [4i32, 5i32]] }

def take_arrint (l: i64) (x: [][]i32): [][]i32 =
  let (v1, _) = split (l) (x) in v1
def reshape_int (l: i64) (x: []i32): []i32 =
  let roundUp = ((l + (length x - 1)) / length x) in
  let extend = flatten (replicate (roundUp) (x)) in
  let (v1, _) = split (l) (extend) in
  v1
entry main (x: i64) (y: i64): [][]i32 =
  let t_v1 = unflatten x y (reshape_int ((x * (y * 1))) (map (\x ->
                                                                                                                     (i32.i64 x + 1)) (iota (6)))) in
  let t_v2 = transpose (t_v1) in
  let t_v3 = take_arrint (x) (t_v2) in
  let t_v4 = transpose (t_v3) in
  t_v4
