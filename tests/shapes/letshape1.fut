-- ==
-- error: Size \[n\] unused

let main (xs: []i32) =
  let [n] xs' = filter (>0) xs
  in n
