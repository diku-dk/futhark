import "/futlib/math"

module type complex = {
  type real
  type complex

  val mk: real -> real -> complex
  val mk_re: real -> complex
  val mk_im: real -> complex

  val conj: complex -> complex
  val re: complex -> real
  val im: complex -> real

  val mag: complex -> real
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
  val to_i32: complex -> i32
}

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
  let to_i32 ((a,_): complex) = T.to_i32 a
}
