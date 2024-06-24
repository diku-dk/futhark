entry main (m: i64) (b: i64) 
              (xs: *[m][b]u32) : *[m*b]u32 =
  let inds = map (map (\ x -> i64.u32 (x*x) )) xs
  let vals = map (map (\ x -> 2*x)) xs  

  let inds' = flatten inds
  let vals' = flatten vals
  
  let inds'' = map2 (\i x -> x*i) (iota (m*b)) inds'
  let vals'' = map2 (\x i -> x / u32.i64 (i+1)) vals' (iota (m*b))
  
  in  scatter (replicate (m*b) 0u32) inds'' vals''

