let main [n][m][k] (arr : [n][m][k]f32) : [n][m][k]f32 =
  let f _ xs = xs[0] + xs[1]
  in stencil_3d [(0,0,0),(-1,-1,-1)] f (map (map (map (const ()))) arr) arr
