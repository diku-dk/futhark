-- We assigned overly complex (and wrong) index functions to splits.
--
-- ==
-- input { 3 4 }
-- output { [1i32, 2i32, 5i32, 6i32, 9i32, 10i32] }

include futlib.numeric

fun take_arrint (l: i32) (x: [][]i32): [][]i32 =
  if (0 <= l)
  then if (l <= (shape (x))[0])
  then let (v1, _) = split (l) (x) in
  v1
  else concat (x) (replicate ((I32.abs (l) - (shape (x))[0])) (replicate ((shape (x))[1]) (0)))
  else if (0 <= (l + (shape (x))[0]))
  then let (_, v2) = split ((l + (shape (x))[0])) (x) in
  v2
  else concat (replicate ((I32.abs (l) - (shape (x))[0])) (replicate ((shape (x))[1]) (0))) (x)
fun reshape_int (l: i32) (x: []i32): []i32 =
  let roundUp = ((l + ((shape (x))[0] - 1)) / (shape (x))[0]) in
  let extend = reshape ((((shape (x))[0] * roundUp))) (replicate (roundUp) (x)) in
  let (v1, _) = split (l) (extend) in
  v1
entry main (n: i32, m: i32): []i32 =
  let t_v1 = reshape ((n,
                       m)) (reshape_int ((n * (m * 1))) (reshape (((shape (map (\(x: i32): i32 ->
                                                                                (x + 1)) (iota (n*m))))[0] * 1)) (map (\(x: i32): i32 ->
                                                                                                                      (x + 1)) (iota (12))))) in
  let t_v2 = rearrange (1, 0) (t_v1) in
  let t_v3 = take_arrint (2) (t_v2) in
  let t_v4 = rearrange (1, 0) (t_v3) in
  reshape (((shape (t_v4))[0] * ((shape (t_v4))[1] * 1))) (t_v4)
