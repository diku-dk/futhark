let f _ (xs: [5]f32) =
  let rx = (xs[0] + xs[1] + xs[2] + xs[3] + xs[4])
  in (rx, 2*rx)

let smoothened () = map (\xs -> stencil_1d ([-2,-1,0,1,2]) f (map (const ()) xs) xs)
                        (tabulate_2d 8 9 (\i j -> f32.i64 (i+j)))

let main = map (map (\(x,y) -> x+y)) (smoothened ())
