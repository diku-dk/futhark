def hof [n] (f: i64 -> i64) (irf: [n]f32) (b: bool) = n

def main [n] [m] (irf: [n]f32) (bs: [m]bool) =
  map (hof id irf) bs
