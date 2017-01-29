import "futlib/numeric"

module type COMPLEX = {
  type real
  type complex

  val mk: real -> real -> complex
  val conj: complex -> complex
  val re: complex -> real
  val im: complex -> real

  val +: complex -> complex -> complex
  val -: complex -> complex -> complex
  val *: complex -> complex -> complex
  val /: complex -> complex -> complex

  val sqrt: complex -> complex

  val fromInt: i32 -> complex
  val fromFraction: i32 -> i32 -> complex
  val toInt: complex -> i32
}

module Complex(T: REAL) = {
  type real = T.t
  type complex = (T.t, T.t)

  fun mk (a: real) (b: real) = (a,b)
  fun conj ((a,b): complex) = (a, T.fromInt 0 T.- b)
  fun re ((a,_b): complex) = a
  fun im ((_a,b): complex) = b

  fun ((a,b): complex) + ((c,d): complex) = (a T.+ c, b T.+ d)
  fun ((a,b): complex) - ((c,d): complex) = (a T.- c, b T.- d)
  fun ((a,b): complex) * ((c,d): complex) = (a T.* c T.- b T.* d,
                                             b T.* c T.+ a T.* d)
  fun ((a,b): complex) / ((c,d): complex) =
    ((a T.* c T.+ b T.* d) T./ (c T.* c T.+ d T.* d),
     (b T.* c T.- a T.* d) T./ (c T.* c T.+ d T.* d))

  fun sqrt ((a,b): complex) =
    let gamma = T.sqrt ((a T.+ T.sqrt (a T.* a T.+ b T.* b)) T./
                        T.fromInt 2)
    let delta = T.fromInt (T.sgn b) T.*
                T.sqrt (((T.fromInt 0 T.- a) T.+
                         T.sqrt (a T.* a T.+ b T.* b)) T./
                        T.fromInt 2)
    in (gamma, delta)

  fun fromFraction (a: i32) (b: i32): complex =
    mk (T.fromFraction a b) (T.fromInt 0)
  fun fromInt (a: i32) = fromFraction a 1
  fun toInt ((a,_): complex) = T.toInt a
}
