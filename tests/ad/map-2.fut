-- Simple reduce with multiplication
-- ==

-- compiled input { [1.0f32, 2.0f32, 3.0f32, 4.0f32] [5.0f32, 6.0, 7.0, 8.0]} output { [24.0f32, 12.0f32, 8.0f32, 6.0f32] }

let map_mult [n][m] (xs: [n]f32) : [m]f32 =
  let a = xs[0] in
  map (\i -> 
  			let x2 = xs[i]
  			let x1 = xs[i-1]--if i>0 then xs[i-1] else x2
  			let x3 = xs[i+1]--if i<n-1 then xs[i+1] else x2
  			in  (x1 + x2 + x3)*a
  			--in x2*x2*a 
  	  ) <| map (+1) (iota m)

entry main [n][m] (xs: [n]f32) (rs_: [m]f32) =
  vjp map_mult xs rs_
