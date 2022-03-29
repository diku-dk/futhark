-- | Module types for Fast Fourier Transforms (FFTs), as well as
-- transformations for automatically performing higher-dimensional
-- FFTs.  For specific FFT implementations, see
-- e.g. `stockham-radix-2`.

module type fft_1d = {
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
}

module type fft_2d = {
  type real

  -- | Perform a forward 2D FFT using the row-column algorithm.
  val fft2 [n][m]: [n][m](real, real) -> [n][m](real, real)
  -- | The inverse of `fft2`@term.
  val ifft2 [n][m]: [n][m](real, real) -> [n][m](real, real)

  -- | Perform a forward 2D FFT of an array of numbers, each representing
  -- the real part of a complex number.
  val fft2_re [n][m]: [n][m]real -> [n][m](real, real)
  -- | The inverse of `fft2_re`@term.
  val ifft2_re [n][m]: [n][m]real -> [n][m](real, real)
}
