def main (a: [4]u64) (b: [4]u64) (c: [4]u64) =
  (\p ->
     let x = length (p.0)
     let y = length (p.1.0)
     let z = length (p.1.1)
     in x < y && y < z) (a, (b, c))
