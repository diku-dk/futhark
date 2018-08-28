-- The kernel extractor tried to distribute "irregular rotations", or
-- whatever you want to call them.

let main [n][m] (xsss: [n][m][]i32) =
  map (map2 (\r xs -> rotate r xs) (iota m)) xsss
