-- | An implementation of complex numbers.  Divided into a module type
-- for modules that implement complex numbers, and a parametric module
-- that can construct such modules.

import "/futlib/math"

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

  val from_i32: i32 -> complex
  val from_fraction: i32 -> i32 -> complex
}

-- | Given a module describing a number type, construct a module
-- implementing complex numbers.
module complex(T: real): (complex with real = T.t) = {
  type real = T.t
  type complex = (T.t, T.t)

  let mk (a: real) (b: real) = (a,b)
  let mk_re (a: real) = (a, T.from_i32 0)
  let mk_im (b: real) = (T.from_i32 0, b)

  let conj ((a,b): complex) = (a, T.from_i32 0 T.- b)
  let re ((a,_b): complex) = a
  let im ((_a,b): complex) = b

  let ((a,b): complex) + ((c,d): complex) = (a T.+ c, b T.+ d)
  let ((a,b): complex) - ((c,d): complex) = (a T.- c, b T.- d)
  let ((a,b): complex) * ((c,d): complex) = (a T.* c T.- b T.* d,
                                             b T.* c T.+ a T.* d)
  let ((a,b): complex) / ((c,d): complex) =
    ((a T.* c T.+ b T.* d) T./ (c T.* c T.+ d T.* d),
     (b T.* c T.- a T.* d) T./ (c T.* c T.+ d T.* d))

  let mag ((a,b): complex) =
    T.sqrt (a T.* a T.+ b T.* b)
  let arg ((a,b): complex) =
    T.atan2 b a

  let sqrt ((a,b): complex) =
    let gamma = T.sqrt ((a T.+ T.sqrt (a T.* a T.+ b T.* b)) T./
                        T.from_i32 2)
    let delta = T.from_i32 (T.to_i32 (T.sgn b)) T.*
                T.sqrt (((T.from_i32 0 T.- a) T.+
                         T.sqrt (a T.* a T.+ b T.* b)) T./
                        T.from_i32 2)
    in (gamma, delta)

  let exp ((a,b): complex) =
    let expx = T.exp a
    in mk (expx T.* T.cos b) (expx T.* T.sin b)

  let log (z: complex) =
    mk (T.log (mag z)) (arg z)

  let from_fraction (a: i32) (b: i32): complex =
    mk (T.from_fraction a b) (T.from_i32 0)
  let from_i32 (a: i32) = from_fraction a 1
}
