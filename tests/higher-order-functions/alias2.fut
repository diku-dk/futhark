let main [h][w] (ether: [h][w]f32) (is: []i32): [][]f32 =
  let ether_flat = copy (flatten ether)
  let vs = map (\i -> ether_flat[i]) is
  in unflatten h w (scatter ether_flat is vs)
