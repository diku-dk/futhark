-- Reshaping a concatenation.
-- ==
-- input { 3i64 [1] [2,3] } output { [1, 2, 3] }
-- input { 4i64 [1] [2,3] } error: cannot match shape
-- structure { Reshape 0 }

def main n xs ys : []i32 =
  xs ++ ys :> [n]i32
