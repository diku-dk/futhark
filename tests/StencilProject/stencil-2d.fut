let main [n] [m] (arr : [n][m]f32) : [n][m]f32 =
  let f () xs = xs[0] + xs[1]
  in stencil_2d [(0,0),(-1,-1)] f (map (map (const ())) arr) arr
