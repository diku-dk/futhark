-- ==
-- tags { no_webgpu }

type complex = {r: f32, i: f32}
def complex r i : complex = {r, i}
def real r = complex r 0

def conjC (a: complex) : complex = {r = a.r, i = -a.i}

def addC (a: complex) (b: complex) : complex = {r = a.r + b.r, i = a.i + b.i}
def subC (a: complex) (b: complex) : complex = {r = a.r - b.r, i = a.i - b.i}
def mulC (a: complex) (b: complex) : complex = {r = a.r * b.r - a.i * b.i, i = a.r * b.i + a.i * b.r}

def pi : f32 = 3.141592653589793

def gfft [n] (inverse: bool) (xs: [n]complex) : [n]complex =
  let logN = assert (i64.popc n == 1) (i64.ctz n)
  let startTheta = pi * f32.from_fraction (2 - (i64.bool inverse << 2)) n
  let ms = n >> 1
  let iteration [l] ((xs: [l]complex), e, theta0) =
    let modc = (1 << e) - 1
    let xs' =
      tabulate l (\i ->
                    let q = i & modc
                    let p' = i >> e
                    let p = p' >> 1
                    let ai = q + (p << e)
                    let bi = ai + ms
                    let a = xs[ai]
                    let b = xs[bi]
                    let theta = theta0 * f32.i64 p
                    in if bool.i64 (p' & 1)
                       then mulC (complex (f32.cos theta) (-f32.sin theta)) (subC a b)
                       else addC a b)
    in (xs', e + 1, theta0 * 2)
  in (iterate logN iteration (xs, 0, startTheta)).0

def gfft3 [m] [n] [k] inverse (A: [m][n][k]complex) =
  let A' = tabulate_2d n k (\i j -> gfft inverse A[:, i, j])
  let A'' = tabulate_2d k m (\i j -> gfft inverse A'[:, i, j])
  let A''' = tabulate_2d m n (\i j -> gfft inverse A''[:, i, j])
  in A'''

def ifft3 [m] [n] [k] (x: [m][n][k]complex) =
  let f = real (f32.from_fraction 1 (m * n * k))
  in gfft3 true x |> map (map (map (mulC f)))

def main = map ifft3
