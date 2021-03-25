-- ==
-- error: Causality

let main : []i32 =
  ([1]++) <| loop (xs: []i32) = [] for i < 10 do (xs ++ xs)
