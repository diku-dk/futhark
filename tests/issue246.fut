-- We assigned overly complex (and wrong) index functions to splits.
--
-- ==
-- input { 3i64 4i64 }
-- output { [1i64, 2i64, 5i64, 6i64, 9i64, 10i64] }


def dim_2 't [d0] [d1] (i: i64) (x: [d0][d1]t): i64 =
  if (i == 1)
  then d1
  else d0

def take_arrint [k] (l: i64) (x: [][k]i64): [][]i64 =
  if (0 <= l)
  then if (l <= length x)
  then let v1 = take (l) (x) in
  v1
  else concat (x) (replicate ((i64.abs (l) - length x)) (replicate (dim_2 1 x) (0) :> [k]i64))
  else if (0 <= (l + length x))
  then let v2 = drop ((l + length x)) (x) in
  v2
  else concat (replicate ((i64.abs (l) - length x)) (replicate (dim_2 1 x) (0) :> [k]i64)) (x)
def reshape_int (l: i64) (x: []i64): []i64 =
  let roundUp = ((l + (length x - 1)) / length x) in
  let extend = flatten (replicate (roundUp) (x)) in
  let v1 = take (l) (extend) in
  v1
entry main (n: i64) (m: i64): []i64 =
  let t_v1 = unflatten (reshape_int ((n * m)) ((map (\(x: i64): i64 ->
                                                                                                                      (x + 1)) (iota (12))))) in
  let t_v2 = transpose (t_v1) in
  let t_v3 = take_arrint (2) (t_v2) in
  let t_v4 = transpose (t_v3) in
  flatten (t_v4)
