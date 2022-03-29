-- | Complex numbers parameterised over the representation of their real parts.
--
-- Divided into a module type for modules that implement complex
-- numbers, and a parametric module that can construct such modules.

-- | The type of modules that implement a notion of complex numbers.
-- Semantically, a complex number can be seen as a pair of two numbers
-- (but this need not be the representation).
module type complex = {
  -- | The type of the components of the complex number.
  type real
  -- | The type of complex numbers.
  type complex

  -- | Construct a complex number from real and imaginary components.
  val mk: real -> real -> complex
  -- | Construct a complex number from just the real component.  The
  -- imaginary part will be zero.
  val mk_re: real -> complex
  -- | Construct a complex number from just the imaginary component.  The
  -- real part will be zero.
  val mk_im: real -> complex

  -- | Conjugate a complex number.
  val conj: complex -> complex
  -- | The real part of a complex number.
  val re: complex -> real
  -- | The imaginary part of a complex number.
  val im: complex -> real

  -- | The magnitude (or modulus, or absolute value) of a complex number.
  val mag: complex -> real
  -- | The argument (or phase) of a complex number.
  val arg: complex -> real

  val +: complex -> complex -> complex
  val -: complex -> complex -> complex
  val *: complex -> complex -> complex
  val /: complex -> complex -> complex

  val sqrt: complex -> complex
  val exp: complex -> complex
  val log: complex -> complex
}

-- | Given a module describing a number type, construct a module
-- implementing complex numbers.
module mk_complex(T: real): (complex with real = T.t
                                     with complex = (T.t, T.t)) = {
  type real = T.t
  type complex = (T.t, T.t)

  def mk (a: real) (b: real) = (a,b)
  def mk_re (a: real) = (a, T.i32 0)
  def mk_im (b: real) = (T.i32 0, b)

  def conj ((a,b): complex) = T.((a, i32 0 - b))
  def re ((a,_b): complex) = a
  def im ((_a,b): complex) = b

  def ((a,b): complex) + ((c,d): complex) = T.((a + c, b + d))
  def ((a,b): complex) - ((c,d): complex) = T.((a - c, b - d))
  def ((a,b): complex) * ((c,d): complex) = T.((a * c - b * d,
                                                b * c + a * d))
  def ((a,b): complex) / ((c,d): complex) =
    T.(((a * c + b * d) / (c * c + d * d),
        (b * c - a * d) / (c * c + d * d)))

  def mag ((a,b): complex) =
    T.(sqrt (a * a + b * b))
  def arg ((a,b): complex) =
    T.atan2 b a

  def sqrt ((a,b): complex) =
    let gamma = T.(sqrt ((a + sqrt (a * a + b * b)) / i32 2))
    let delta = T.(sgn b *
                   sqrt (((i32 0 - a) + sqrt (a * a + b * b)) / i32 2))
    in (gamma, delta)

  def exp ((a,b): complex) =
    let expx = T.exp a
    in mk T.(expx * cos b) T.(expx * sin b)

  def log (z: complex) =
    mk (T.log (mag z)) (arg z)
}
