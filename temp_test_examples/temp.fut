entry main [n][m] (as: [n][m]i32): ([n][m]i32, [n][m]i32) =
  (scan (map2 (+)) (replicate m 0) as, scan (map2 (*)) (replicate m 1) as) 
