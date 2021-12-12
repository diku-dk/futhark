type^ network 'p =
  { zero: p
  , sum: (k:i64) -> [k]p -> p }

def chain 'p1 'p2 (a:network p1) (b:network p2) : network (p1, p2) =
  let zero = (a.zero, b.zero)
  let sum k ps = let (as, bs) = unzip ps
                 in (a.sum k as, b.sum k bs)
  in {zero, sum}

def linear [m][n] (weights:[m][n]f32) : network ([m][n]f32) =
  let zero = replicate m (replicate n 0)
  let sum k (ps:[k][m][n]f32) = map (map (reduce (+) 0)) (map transpose (transpose ps))
  in {zero, sum}

def main [m][n] (ws1: [n][m]f32) (ws2: [n][n]f32) =
  let inet = chain (linear ws1) (linear ws2)
  in inet.zero
