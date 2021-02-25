-- Simple reduce with multiplication
-- ==
-- compiled input { [1.0f32, 2.0f32, 3.0f32, 4.0f32] [5.0f32, 4.0f32, 3.0f32, 2.0f32] } output { [79.0f32, 37.0f32, 22.0f32, 12.0f32] }

let scan_vec_add_mult [m] (xss: [m][2](f32,f64)) : [m][2](f32,f64) =
  scan  (\ ac xs -> let (a32, a64) = unzip ac
  		 			let (x32, x64) = unzip xs
  					let r32 = [ a32[0]+x32[0], a32[1]+x32[1] ]
  					let r64 = [ a64[0]*x64[0], a64[1]*x64[1] ]
  					in  zip r32 r64
        ) (replicate 2 (0f32,1f64)) xss

entry main [m] (xss1: [m][2]f32) (xss2: [m][2]f64) (yss_bar1: [m][2]f32) (yss_bar2: [m][2]f64) : ([m][2]f32, [m][2]f64) =
  let xss     = map2 zip xss1 xss2
  let yss_bar = map2 zip yss_bar1 yss_bar2 
  let res = vjp scan_vec_add_mult xss yss_bar
  in  unzip (map unzip res)
