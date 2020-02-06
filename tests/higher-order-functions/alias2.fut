let main [h][w][n] (ether: [h][w]f32) (is: [n]i32): [][]f32 =
  let ether_flat = copy (flatten ether)
  let vs = map (\i -> unsafe ether_flat[i]) is
  in unflatten h w (scatter ether_flat is vs)
