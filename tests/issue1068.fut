type complex = {r: f32, i: f32}
def complex r i : complex = {r, i}
def real r = complex r 0
def imag i = complex 0 i
def zero = complex 0 0

def addC (a: complex) (b: complex) : complex = {r = a.r + b.r, i = a.i + b.i}
def subC (a: complex) (b: complex) : complex = {r = a.r - b.r, i = a.i - b.i}
def mulC (a: complex) (b: complex) : complex = {r = a.r * b.r - a.i * b.i, i = a.r * b.i + a.i * b.r}

def divC (a: complex) (b: complex) : complex =
  let d = b.r * b.r + b.i * b.i
  let r = (a.r * b.r + a.i * b.i) / d
  let i = (a.i * b.r - a.r * b.i) / d
  in {r, i}

def pi : f32 = 3.141592653589793

def gfft [n] (inverse: bool) (xs: [n]complex) : [n]complex =
  let dir = 1 - 2 * i64.bool inverse
  let (n', iter) =
    iterate_while ((< n) <-< (.0)) (\(a, b) -> (a << 1, b + 1)) (1, 0)
  let iteration [l] ((xs: [l]complex), m, e, theta0) =
    let modc = (1 << e) - 1
    let xs' =
      tabulate l (\i ->
                    let i = i32.i64 i
                    let q = i & modc
                    let p' = i >> e
                    let p = p' >> 1
                    let a = xs[q + (p << e)]
                    let b = xs[q + (p + m << e)]
                    let theta = theta0 * f32.i32 p
                    in if bool.i32 (p' & 1)
                       then mulC (complex (f32.cos theta) (-f32.sin theta)) (subC a b)
                       else addC a b)
    in (xs', m >> 1, e + 1, theta0 * 2)
  in (iterate iter iteration (xs, i32.i64 (n >> 1), 0, pi * f32.from_fraction (dir * 2) n) |> (.0))

def gfft3 [m] [n] [k] inverse (A: [m][n][k]complex) =
  tabulate_2d n k (\i j -> gfft inverse A[:, i, j])

def main testData =
  gfft3 false (map (map (map real)) testData)
