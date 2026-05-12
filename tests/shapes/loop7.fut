-- Infer correctly that the loop parameter 'ys' has a variant size.
-- ==
-- input { [0i64,1i64] } output { 2i64 [0i64] }

def first_nonempty (f: i64 -> []i64) xs =
  loop (i, ys) = (0, [] : []i64)
  while null ys && i < length xs do
    let i' = i + 1
    let ys' = f xs[i]
    in (i', ys')

def main [n] (xs: [n]i64) =
  first_nonempty iota xs
