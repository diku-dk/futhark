module complex (n: real) = {
  type t = {r: n.t, i: n.t}
  def complex r i : t = {r, i}
  def real r = complex r (n.f32 0)
  def imag i = complex (n.f32 0) i
  def zero = real (n.f32 0)
  def one = real (n.f32 1)
  def neg (c: t) = complex (n.neg c.r) (n.neg c.i)
  def conj (c: t) = complex (c.r) (n.neg c.i)
  def scale f (c: t) : t = complex (f n.* c.r) (f n.* c.i)
  def imagScale f (c: t) : t = complex (n.neg f n.* c.i) (f n.* c.r)
  def add (a: t) (b: t) : t = complex (a.r n.+ b.r) (a.i n.+ b.i)
  def sub (a: t) (b: t) : t = complex (a.r n.- b.r) (a.i n.- b.i)
  def mul (a: t) (b: t) : t = complex (a.r n.* b.r n.- a.i n.* b.i) (a.r n.* b.i n.+ a.i n.* b.r)

  def div (a: t) (b: t) : t =
    let f = n.recip (b.r n.* b.r n.+ b.i n.* b.i)
    let r = f n.* (a.r n.* b.r n.+ a.i n.* b.i)
    let i = f n.* (a.i n.* b.r n.- a.r n.* b.i)
    in {r, i}

  def abs (c: t) : n.t = c.r n.* c.r n.+ c.i n.* c.i

  def inv (c: t) =
    let f = n.recip (c.r n.* c.r n.+ c.i n.* c.i)
    in scale f (conj c)
}

module fft (r: real) = {
  module C = complex r
  type complex = C.t
  type real = r.t
  def real = C.real

  def fromReal = real
  def fromReal1d = map fromReal
  def fromReal2d = map fromReal1d
  def fromReal3d = map fromReal2d

  def toReal : (complex -> real) = (.r)
  def toReal1d = map toReal
  def toReal2d = map toReal1d
  def toReal3d = map toReal2d

  def gfft [n] (inverse: bool) (xs: [n]complex) : [n]complex =
    let logN = assert (i64.popc n == 1) (i64.i32 <| i64.ctz n)
    in #[unsafe]
       let startTheta = r.pi r.* r.from_fraction (2 - (i64.bool inverse << 2)) n
       let ms = n >> 1
       in (loop (input: *[n]complex, output: *[n]complex, theta0) = (copy xs, copy xs, startTheta)
           for logS < logN do
             let s = 1 << logS
             let modc = s - 1
             let logS' = logS + 1
             let as = input[:ms]
             let bs = input[ms:]
             let (cs, cis, ds, dis) =
               map3 (\i a b ->
                       let q = i & modc
                       -- i % s
                       let p = i >> logS
                       -- i / s
                       let ci = q + (p << logS')
                       -- q + s * p * 2
                       let di = ci + s
                       -- q + s *(p * 2 + 1)
                       let theta = theta0 r.* r.i64 p
                       let c = C.add a b
                       let d = C.mul (C.complex (r.cos theta) (r.neg (r.sin theta))) (C.sub a b)
                       in (c, ci, d, di))
                    (iota ms)
                    as
                    (bs :> [ms]complex)
               |> unzip4
             let input' =
               scatter output
                       (cis ++ dis :> [n]i64)
                       (cs ++ ds :> [n]complex)
             in (input', input, theta0 r.* r.i32 2)).0

  def gfft2 [m] [n] inverse (A: [m][n]complex) =
    let A' = tabulate n (\i -> gfft inverse A[:, i])
    let A'' = tabulate m (\i -> gfft inverse A'[:, i])
    in A''

  def gfft3 [m] [n] [k] inverse (A: [m][n][k]complex) =
    let A' = tabulate_2d n k (\i j -> gfft inverse A[:, i, j])
    let A'' = tabulate_2d k m (\i j -> gfft inverse A'[:, i, j])
    let A''' = tabulate_2d m n (\i j -> gfft inverse A''[:, i, j])
    in A'''
}

module fft32 = fft f64

entry bench3d32 (A: [][][]f64) =
  #[unsafe]
  map (map (map fft32.real)) A
  |> fft32.gfft3 false
