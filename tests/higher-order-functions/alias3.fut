type pair = (f32,i32)

let main [h][w][n] (ether: [h][w]pair) (is: [n]i32): [h][w]pair =
  let ether_flat = copy (flatten ether)
  let vs = map (\i -> unsafe ether_flat[i]) is
  in unflatten h w (scatter ether_flat is vs)
