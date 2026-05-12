-- ==
-- compiled random input { [20000]f32 } error: l != 1337

def main [n] (xs: *[n]f32) =
  let xs[1337] = f32.lowest
  let op i j =
    let l = if xs[i] < xs[j] then i else j
    in assert (l != 1337) l
  in hist op 0 3 (map (% 3) (iota n)) (iota n)
