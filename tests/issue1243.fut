def DistMatrix [m] (b: [m]f32) : [m][m]f32 =
  let initial = replicate m b
  let outside = (replicate (m + 1) f32.inf) with [0] = 0
  in (loop (D, column) = (initial, outside)
      for i < m do
        let next_row =
          loop cs = replicate (m + 1) f32.inf
          for j in 1...m do
            cs with [j] = f32.minimum [cs[j - 1], column[j]]
        in (D with [i] = (next_row[1:] :> [m]f32), next_row))
     |> \(x, _) -> x

def main [d] [n] (s: [d][n]f32) as =
  map (\a -> #[sequential] DistMatrix s[a]) as
