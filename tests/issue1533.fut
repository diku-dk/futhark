-- In OpenCL backend: subtle bug caused by interchange not respecting
-- permutations.
-- In multicore backend: invalid hoisting after double buffering.
-- ==
-- compiled random input { [20][20][20]f64 } auto output

type complex = {r: f64, i: f64}
def complex r i : complex = {r, i}
def real r = complex r 0
def imag i = complex 0 i
def zero = complex 0 0

def conjC (a: complex) : complex = {r = a.r, i = -a.i}

def addC (a: complex) (b: complex) : complex = {r = a.r + b.r, i = a.i + b.i}
def subC (a: complex) (b: complex) : complex = {r = a.r - b.r, i = a.i - b.i}
def mulC (a: complex) (b: complex) : complex = {r = a.r * b.r - a.i * b.i, i = a.r * b.i + a.i * b.r}

type triad 't = (t, t, t)
def triadMap 'a 'b (f: a -> b) (A: triad a) : triad b = (f A.0, f A.1, f A.2)
def triadMap2 'a 'b 'c (f: a -> b -> c) (A: triad a) (B: triad b) : triad c = (f A.0 B.0, f A.1 B.1, f A.2 B.2)
def triadFold 'a (f: a -> a -> a) (A: triad a) : a = f A.0 <| f A.1 A.2

type v3 = triad f64

def v3sum (v: v3) : f64 = triadFold (+) v
def v3add (a: v3) (b: v3) : v3 = triadMap2 (+) a b
def v3sub (a: v3) (b: v3) : v3 = triadMap2 (-) a b
def v3mul (a: v3) (b: v3) : v3 = triadMap2 (*) a b
def v3dot (a: v3) (b: v3) : f64 = v3mul a b |> v3sum
def scaleV3 (f: f64) = triadMap (* f)
def v3abs a = f64.sqrt (v3dot a a)

def fromReal : (f64 -> complex) = real
def fromReal1d = map fromReal
def fromReal2d = map fromReal1d
def fromReal3d = map fromReal2d

def toReal : (complex -> f64) = (.r)
def toReal1d = map toReal
def toReal2d = map toReal1d
def toReal3d = map toReal2d

def gfft [n] (xs: [n]complex) : [n]complex =
  let startTheta = f64.pi * f64.from_fraction 2 n
  let ms = n >> 1
  let iteration ((xs: [n]complex), e, theta0) =
    let modc = (1 << e) - 1
    let xs' =
      tabulate n (\i ->
                    let q = i & modc
                    let p' = i >> e
                    let p = p' >> 1
                    let ai = q + (p << e)
                    let bi = ai + ms
                    let a = xs[ai]
                    let b = xs[bi]
                    let theta = theta0 * f64.i64 p
                    in mulC (complex (f64.cos theta) (-f64.sin theta)) (subC a b))
    in (xs', e + 1, theta0 * 2)
  in (iterate 2 iteration (xs, 0, startTheta)).0

def fft3 [m] [n] [k] (A: [m][n][k]complex) =
  #[unsafe]
  #[incremental_flattening(only_inner)]
  tabulate_2d n k (\i j -> gfft A[:, i, j])

entry main grid = fromReal3d grid |> fft3 |> toReal3d
