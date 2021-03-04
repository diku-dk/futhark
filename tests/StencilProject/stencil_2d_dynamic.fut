let main [n][m] (arr : [n][m]f32) (ixs:[2]i64) =
  let ixss = zip ixs ixs
  let f _ xs = xs[0] + xs[1]
  in stencil_2d ixss f (map (map (const ())) arr) arr
