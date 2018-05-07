-- | A simple FFT module based on work by David P.H. Jørgensen and
-- Kasper Abildtrup Hansen.

import "/futlib/complex"

module type fft = {
  type real

  -- | Perform a forward FFT on an array of numbers, each being
  -- represented as the pair of the real and imaginary part of a
  -- complex number.
  val fft [n]: [n](real, real) -> [n](real, real)
  -- | The inverse of `fft`@term.
  val ifft [n]: [n](real, real) -> [n](real, real)

  -- | Perform a forward FFT of an array of numbers, each representing
  -- the real part of a complex number.
  val fft_re [n]: [n]real -> [n](real, real)
  -- | The inverse of `fft_re`@term.
  val ifft_re [n]: [n]real -> [n](real, real)

  -- | Perform a forward 2D FFT using the row-column algorithm.
  val fft2 [n][m]: [n][m](real, real) -> [n][m](real, real)
  -- | The inverse of `fft2`@term.
  val ifft2 [n][m]: [n][m](real, real) -> [n][m](real, real)

  -- | Perform a forward 2D FFT of an array of numbers, each representing
  -- the real part of a complex number.
  val fft2_re [n][m]: [n][m]real -> [n][m](real, real)
  -- | The inverse of `2fft_re`@term.
  val ifft2_re [n][m]: [n][m]real -> [n][m](real, real)
}

-- | Given a module describing real numbers, produce a module for
-- performing FFTs using Stockham's algorithm.  All of these will pad
-- with zeroes up to the next power of two before carrying out the
-- computation, and return the truncated array.
module mk_fft (R: real): fft with real = R.t = {
  module complex = complex R
  type real = R.t
  type complex = complex.complex

  let radix = 2

  let fft_iteration [n] (forward: R.t) (ns: i32) (data: [n]complex) (j: i32)
                  : (i32, complex, i32, complex) =
    let angle = R.(f64(-2.0) * forward * pi * (i32(j % ns))) R./ R.i32(ns * radix)
    let (v0, v1) = (data[j],
                    data[j+n/radix] complex.* (complex.mk (R.cos angle) (R.sin angle)))

    let (v0, v1) =  (v0 complex.+ v1, v0 complex.- v1)
    let idxD = ((j/ns)*ns*radix) + (j % ns)
    in (idxD, v0, idxD+ns, v1)

  let fft' [n] (forward: R.t) (input: [n]complex) (bits: i32) : []complex =
    let input = flatten (copy (unflatten (n/2) 2 input))
    let output = flatten (copy (unflatten (n/2) 2 input))
    let ix = iota(n/radix)
    let NS = map (radix**) (iota bits)
    let (res,_) =
      loop (input': *[n]complex, output': *[n]complex) = (input, output) for ns in NS do
        let (i0s, v0s, i1s, v1s) =
          unsafe (unzip (map (fft_iteration forward ns input') ix))
        in (scatter output' (concat i0s i1s) (concat v0s v1s), input')
    in res

  let log2 (n: i32) : i32 =
    let r = 0
    let (r, _) = loop (r,n) while 1 < n do
      let n = n / 2
      let r = r + 1
      in (r,n)
    in r

  let next_pow_2 (n: i32): (i32, i32) =
    if n == 0 then (0, 0) else let d = log2 n
                               in if n == 2**d then (n, d) else (2**(d+1), d+1)

  let ensure_pow_2 [n] (data: [n](R.t, R.t)): (*[](R.t, R.t), i32) =
    let (m, d) = next_pow_2 n
    in if n == 0 then (copy data, d)
       else (concat data (replicate (m - n) (complex.mk_re (R.i32 0))),
             d)

  let generic_fft [n] (forward: bool) (data: [n](R.t, R.t)): [n](R.t, R.t) =
    let (data', bits) = ensure_pow_2 data
    let forward' = if forward then R.i32 1 else R.i32 (-1)
    in take n (fft' forward' data' bits)

  let fft [n] (data: [n](R.t, R.t)): [n](R.t, R.t) =
    generic_fft true data

  let ifft [n] (data: [n](R.t, R.t)): [n](R.t, R.t) =
    let nc = complex.mk_re (R.i32 n)
    in map (complex./nc) (generic_fft false data)

  let fft_re [n] (data: [n]R.t): [n](R.t, R.t) =
    fft (map complex.mk_re data)

  let ifft_re [n] (data: [n]R.t): [n](R.t, R.t) =
    ifft (map complex.mk_re data)

  let generic_fft2 [n][m] (forward: bool) (data: [n][m](R.t, R.t)): [n][m](R.t, R.t) =
    let zero = complex.mk_re (R.i32 0)
    let (n', n_bits) = next_pow_2 n
    let (m', m_bits) = next_pow_2 m
    let forward' = if forward then R.i32 1 else R.i32 (-1)
    let data = concat (map (\r -> concat r (replicate (m'-m) zero)) data)
                      (replicate (n'-n) (replicate m' zero))
    let data = map (\r -> fft' forward' r m_bits) data
    let data = map (\c -> fft' forward' c n_bits) (transpose data)
    in (transpose data)[:n,:m]

  let fft2 [n][m] (data: [n][m](R.t, R.t)): [n][m](R.t, R.t) =
    generic_fft2 true data

  let ifft2 [n][m] (data: [n][m](R.t, R.t)): [n][m](R.t, R.t) =
    let nc = complex.mk_re (R.i32 (n*m))
    in map (\r -> map (complex./nc) r) (generic_fft2 false data)

  let fft2_re [n][m] (data: [n][m]R.t): [n][m](R.t, R.t) =
    fft2 (map (\r -> map complex.mk_re r) data)

  let ifft2_re [n][m] (data: [n][m]R.t): [n][m](R.t, R.t) =
    ifft2 (map (\r -> map complex.mk_re r) data)

}
