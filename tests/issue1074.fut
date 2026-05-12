def dot [n] (x: [n]bool) (y: [n]bool) : i64 =
  i64.sum (map2 (\x y -> i64.bool <| x && y) x y)

local
def choose (n: i64) (k: i64) : f32 =
  f32.product (map (\i -> f32.i64 (n + 1 - i) / f32.i64 i) (1...k))

def kc [m] (x: [m]bool) (y: [m]bool) (d: i64) : f32 =
  f32.sum (map (choose (dot x y)) (1...d))

def norm [m] (k: [m]bool -> [m]bool -> f32) (x: [m]bool) (y: [m]bool) : f32 =
  let xy = k x y
  let xx = k x x
  let yy = k y y
  in xy / (f32.sqrt xx * f32.sqrt yy)

def kcn x y d = norm (\x y -> kc x y d) x y

type centroid [n] [m] = {d: i64, w: [n]f32, trx: [n][m]bool, try: [n]bool}

def train [n] [m] (x: [n][m]bool) (y: [n]bool) (d: i64) : centroid [n] [m] =
  let zeros = replicate n 0
  let w = reduce (map2 (+)) zeros (map2 (\x' y -> (map (\x'' -> (2 * f32.bool y - 1) * kcn x' x'' d) x)) x y)
  in {d = d, w = w, trx = x, try = y}

def predict [n] [m] [k] (c: centroid [n] [m]) (xs: [k][m]bool) : [k]f32 =
  map (\x -> f32.sum (map2 (\w x' -> w * kcn x x' c.d) c.w c.trx)) xs

def lto [n] [m] (c: centroid [n] [m]) =
  let mean x = f32.sum x / f32.i64 (length x)
  let zero i x = tabulate n (\j -> if j == i then 0 else x[j])
  let cmod i c = {d = c.d, w = zero i c.w, trx = c.trx, try = c.try}
  let score i j =
    if c.try[i] || c.try[i] == c.try[j]
    then -1
    else let c' = cmod i (cmod j c)
         let ys = predict c' [c.trx[i], c.trx[j]]
         in if ys[0] < ys[1] then 1 else if ys[0] == ys[1] then 0.5 else 0
  in tabulate_2d n n score |> flatten |> filter (>= 0) |> mean

entry test [n] [m] (x: [n][m]bool) (y: [n]bool) =
  map (\d -> lto (train x y d)) (1...10)
