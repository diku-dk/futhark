def main [h][w][n] (ether: [h][w]f32) (is: [n]i64): [][]f32 =
  let ether_flat = copy (flatten ether)
  let vs = map (\i -> ether_flat[i]) is
  in unflatten (scatter ether_flat is vs)
