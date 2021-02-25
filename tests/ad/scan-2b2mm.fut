-- Simple reduce with multiplication
-- ==

-- compiled input { [1.0f32, 2.0f32, 3.0f32, 4.0f32] [5.0f32, 4.0f32, 3.0f32, 2.0f32] } output { [79.0f32, 37.0f32, 22.0f32, 12.0f32] }

let mm2by2  (a1: f32, b1: f32, c1: f32, d1: f32)
			(a2: f32, b2: f32, c2: f32, d2: f32) =
	( a1*a2 + b1*c2
	, a1*b2 + b1*d2
	, c1*a2 + d1*c2
	, c1*b2 + d1*d2
	)

let scan_mm [n] (xs: [n](f32,f32,f32,f32)) =
  scan mm2by2 (1f32, 0f32, 0f32, 1f32) xs


entry main [n] (xs1: [n]f32) (xs2: [n]f32) (xs3: [n]f32) (xs4: [n]f32) (yb1: [n]f32) (yb2: [n]f32) (yb3: [n]f32) (yb4: [n]f32) =
  unzip4 <| vjp scan_mm (zip4 xs1 xs2 xs3 xs4) (zip4 yb1 yb2 yb3 yb4)
