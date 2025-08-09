-- ==
-- error: Causality

def main : []i32 =
  ([1] ++) <| loop (xs: []i32) = [] for i < 10 do (xs ++ xs)
