-- scan with vectorized operator
-- ==

let scan_vec_add_mult [m][n] (xss: [m][n](f32,f64)) : [m][n](f32,f64) =
  scan ( map2 (\ (a1,b1) (a2,b2) -> (a1+a2, b1*b2) ) )
       (replicate n (0f32,1f64)) xss

entry main [m][n] (xss1: [m][n]f32) (xss2: [m][n]f64) (yss_bar1: [m][n]f32) (yss_bar2: [m][n]f64) : ([m][n]f32, [m][n]f64) =
  let xss     = map2 zip xss1 xss2
  let yss_bar = map2 zip yss_bar1 yss_bar2 
  let res = vjp scan_vec_add_mult xss yss_bar
  in  unzip (map unzip res)
