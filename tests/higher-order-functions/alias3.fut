type pair = (f32,i32)

def main [h][w][n] (ether: [h][w]pair) (is: [n]i64): [h][w]pair =
  let ether_flat = copy (flatten ether)
  let vs = map (\i -> ether_flat[i]) is
  in unflatten (scatter ether_flat is vs)
