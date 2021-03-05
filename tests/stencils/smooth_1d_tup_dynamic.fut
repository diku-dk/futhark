-- Testing for dynamic stencil index with tuples
-- ==
--
-- input { [1f32,2,3,4,5] [5f64,4,3,2,1] [1u8,2,3,4,5] [2,-1,0,1,-2] } output { [24f32,33,45,57,66] }

let smooth (xs: [](f32,f64,u8)) (ixs : [5]i64) =
  let f () xss =
    let (xs, ys, zs) = unzip3 xss
    let rx = (xs[0] + xs[1] + xs[2] + xs[3] + xs[4])
    let ry = (ys[0] + ys[1] + ys[2] + ys[3] + ys[4])
    let rz = zs[0] + zs[1] + zs[2] + zs[3] + zs[4]
    in (rx+(f32.f64 ry),rz)

  in stencil_1d ixs f (map (const ()) xs) xs

let main [n] (xs : [n]f32) (ys : [n]f64) (zs : [n]fu8) (ixs : [5]i64) = 
	let smoothened = smooth (zip3 xs ys zs) ixs
	in map (\(fl, bt) -> fl + f32.u8 bt) smoothened
