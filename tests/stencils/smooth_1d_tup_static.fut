-- Testing for static stencil index with tuples
-- ==
--
-- input { [1f32,2f32,3f32,4f32,5f32] [5f64,4f64,3f64,2f64,1f64] [1u8,2u8,3u8,4u8,5u8] } output { [38f32, 41f32, 45f32, 49f32, 52f32] }

let smooth (xs: [](f32,f64,u8)) =
  let f () xss =
    let (xs, ys, zs) = unzip3 xss
    let rx = (xs[0] + xs[1] + xs[2] + xs[3] + xs[4])
    let ry = (ys[0] + ys[1] + ys[2] + ys[3] + ys[4])
    let rz = zs[0] + zs[1] + zs[2] + zs[3] + zs[4]
    in (rx+(f32.f64 ry),rz)

  in stencil_1d ([-2,-1,0,1,2]) f (map (const ()) xs) xs

let main [n] (xs : [n]f32) (ys : [n]f64) (zs : [n]u8) = 
	let smoothened = smooth (zip3 xs ys zs)
	in map (\(fl, bt) -> fl + f32.u8 bt) smoothened
