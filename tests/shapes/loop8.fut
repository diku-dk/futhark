-- ==
-- error: Causality

def main : []i32 =
  (.0) <| loop (xs: []i32, j) = ([], 0) for i < 10 do (xs ++ xs, j + 1)
