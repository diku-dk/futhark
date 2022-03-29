-- | A simple FFT module based on work by David P.H. Jørgensen and
-- Kasper Abildtrup Hansen.  Uses a Stockham radix-2 algorithm.

open import "fft"
import "../complex/complex"

-- | Given a module describing real numbers, produce a module for
-- performing FFTs using Stockham's algorithm.  Requires that the
-- input is a power of two; otherwise an error is raised.
module mk_fft (R: real): {
  include fft_1d with real = R.t
  include fft_2d with real = R.t
} = {
  module complex = mk_complex R
  type real = R.t
  type complex = complex.complex

  def radix:i32 = 2

  def fft_iteration [n] (forward: R.t) (ns: i32) (data: [n]complex) (j: i32)
                  : (i64, complex, i64, complex) =
    let angle = R.(f64(-2.0) * forward * pi) R.* (R.i32(j % ns)) R./ R.i32(ns * radix)
    let (v0, v1) = (data[j],
                    data[j+i32.i64 n/radix] complex.* (complex.mk (R.cos angle) (R.sin angle)))

    let (v0, v1) =  (v0 complex.+ v1, v0 complex.- v1)
    let idxD = ((j/ns)*ns*radix) + (j % ns)
    in (i64.i32 idxD, v0, i64.i32 (idxD+ns), v1)

  def fft' [n] (forward: R.t) (input: [n]complex) (bits: i32) : [n]complex =
    let bits64 = i64.i32 bits
    let input = copy input
    let output = copy input
    let ix = iota(n/i64.i32 radix)
    let NS = map (radix**) (map i32.i64 (iota bits64))
    let (res,_) =
      loop (input': *[n]complex, output': *[n]complex) = (input, output) for ns in NS do
        let (i0s, v0s, i1s, v1s) =
          unzip4 (map (fft_iteration forward ns input') (map i32.i64 ix))
        in (scatter output'
                    (i0s ++ i1s :> [n]i64)
                    (v0s ++ v1s :> [n]complex),
            input')
    in res

  def log2 (n: i64) : i32 =
    let r = 0
    let (r, _) = loop (r,n) while 1 < n do
      let n = n / 2
      let r = r + 1
      in (r,n)
    in r

  def is_power_of_2 (x: i64) = (x & (x - 1)) == 0

  def generic_fft [n] (forward: bool) (data: [n](R.t, R.t)): [n](R.t, R.t) =
    assert (is_power_of_2 n)
           (let bits = log2 n
            let forward' = if forward then R.i32 1 else R.i32 (-1)
            in fft' forward' data bits)

  def fft [n] (data: [n](R.t, R.t)): [n](R.t, R.t) =
    generic_fft true data

  def ifft [n] (data: [n](R.t, R.t)): [n](R.t, R.t) =
    let nc = complex.mk_re (R.i64 n)
    in map (complex./nc) (generic_fft false data)

  def fft_re [n] (data: [n]R.t): [n](R.t, R.t) =
    fft (map complex.mk_re data)

  def ifft_re [n] (data: [n]R.t): [n](R.t, R.t) =
    ifft (map complex.mk_re data)

  def generic_fft2 [n][m] (forward: bool) (data: [n][m](R.t, R.t)): [n][m](R.t, R.t) =
    assert (is_power_of_2 n && is_power_of_2 m)
           (let n_bits = log2 n
            let m_bits = log2 m
            let forward' = if forward then R.i32 1 else R.i32 (-1)
            let data = map (\r -> fft' forward' r m_bits) data
            let data = map (\c -> fft' forward' c n_bits) (transpose data)
            in transpose data)

  def fft2 [n][m] (data: [n][m](R.t, R.t)): [n][m](R.t, R.t) =
    generic_fft2 true data

  def ifft2 [n][m] (data: [n][m](R.t, R.t)): [n][m](R.t, R.t) =
    let nc = complex.mk_re (R.i64 (n*m))
    in map (\r -> map (complex./nc) r) (generic_fft2 false data)

  def fft2_re [n][m] (data: [n][m]R.t): [n][m](R.t, R.t) =
    fft2 (map (\r -> map complex.mk_re r) data)

  def ifft2_re [n][m] (data: [n][m]R.t): [n][m](R.t, R.t) =
    ifft2 (map (\r -> map complex.mk_re r) data)
}
