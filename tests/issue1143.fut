def dotprod [n] (xs: [n]f32) (ys: [n]f32) : f32 =
  reduce (+) 0.0 (map2 (*) xs ys)

def house [d] (x: [d]f32) : ([d]f32, f32) =
  let dot = dotprod x x
  let dot' = dot - x[0] ** 2 + x[0] ** 2
  let beta = if dot' != 0 then 2.0 / dot' else 0
  in (x, beta)

def matmul [n] [p] [m] (xss: [n][p]f32) (yss: [p][m]f32) : [n][m]f32 =
  map (\xs -> map (dotprod xs) (transpose yss)) xss

def outer [n] [m] (xs: [n]f32) (ys: [m]f32) : [n][m]f32 =
  matmul (map (\x -> [x]) xs) [ys]

def matsub [m] [n] (xss: [m][n]f32) (yss: [m][n]f32) : *[m][n]f32 =
  map2 (\xs ys -> map2 (-) xs ys) xss yss

def matadd [m] [n] (xss: [m][n]f32) (yss: [m][n]f32) : [m][n]f32 =
  map2 (\xs ys -> map2 (+) xs ys) xss yss

def matmul_scalar [m] [n] (xss: [m][n]f32) (k: f32) : *[m][n]f32 =
  map (map (* k)) xss

def block_householder [m] [n] (A: [m][n]f32) (r: i64) : ([][]f32, [][]f32) =
  #[unsafe]
  let Q = replicate m (replicate m 0)
  let (Q, A) =
    loop (Q, A) = (Q, copy A)
    for k in 0..<(n / r) do
      let s = k * r
      let V = replicate m (replicate r 0f32)
      let Bs = replicate r 0f32
      let (A) =
        loop (A) for j in 0..<r do
          let u = s + j
          let block = A[u:, u:s + r]
          let (v, B) = house block[:, 0]
          let BvvT = (matmul_scalar (outer v v) B)
          let BvvTAk = matmul BvvT block
          let A[u:, u:s + r] = matsub block BvvTAk
          in A
      let Q[:, s:] = copy Q[:, s:]
      in (Q, A)
  in (Q, A)

def main arrs r = map (\arr -> block_householder arr r) arrs
