let smooth (xs: [](f32,f64,u8)) =
  let f () xss =
    let (xs, ys, zs) = unzip3 xss
    let rx = (xs[0] + xs[1] + xs[2] + xs[3] + xs[4])
    let ry = (ys[0] + ys[1] + ys[2] + ys[3] + ys[4])
    let rz = zs[0] + zs[1] + zs[2] + zs[3] + zs[4]
    in (rx,ry,rz)

  in stencil_1d ([-2,-1,0,1,2]) f (map (const ()) xs) xs

let smoothened () = smooth (zip3 [1,2,3,4,5,6,7,8] [9,8,7,6,5,4,3,2] [1,3,5,7,9,11,13,15])

let main = map (\(fl, db, bt) -> fl + f32.f64 db + f32.u8 bt) (smoothened ())
