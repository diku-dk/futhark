-- failing now
-- ==
-- input { 3i64 4i64 } 
-- auto output

def main (m: i64) (n: i64) : [n][n]f32 =
  tabulate n (\j ->
    let zeros = replicate m (replicate n 0.0f32)
    let row = replicate n 10.0f32 with [j] = 1.0f32
    let updated = zeros with [0] = row
    in reduce (map2 (+)) (replicate n 0.0f32) updated)