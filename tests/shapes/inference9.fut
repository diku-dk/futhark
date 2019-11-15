-- Inferring a functional parameter type that refers to a size that is
-- not constructively provided until a later parameter.
-- ==
-- input { [1,2,3,4,5,6,7,8] }

let f [n] sorter (xs: [n]i32) : [n]i32 =
  sorter xs

let main [n] (xs: [n]i32) =
  f id xs
