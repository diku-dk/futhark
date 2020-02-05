-- It is OK to infer stricter constraints than what is
-- provided by explicit size parameters, if present.
-- ==
-- input { [1,2] } output { [1,2] }
-- compiled input { [1,2,3] } error:

let main (xs: []i32) : [2]i32 =
  xs
