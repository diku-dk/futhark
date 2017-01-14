include numeric

module type COMPLEX {
  type real
  type complex

  val mk: real -> real -> complex
  val conj: complex -> complex
  val re: complex -> real
  val im: complex -> real

  val add: complex -> complex -> complex
  val sub: complex -> complex -> complex
  val mul: complex -> complex -> complex
  val div: complex -> complex -> complex

  val sqrt: complex -> complex
}

module Complex(T: REAL): COMPLEX {
  type real = T.t
  type complex = (T.t, T.t)

  fun mk (a: real) (b: real) = (a,b)
  fun conj ((a,b): complex) = (a, T.sub (T.fromInt 0) b)
  fun re ((a,_b): complex) = a
  fun im ((_a,b): complex) = b

  fun add ((a,b): complex) ((c,d): complex) = (T.add a c, T.add b d)
  fun sub ((a,b): complex) ((c,d): complex) = (T.sub a c, T.sub b d)
  fun mul ((a,b): complex) ((c,d): complex) = (T.sub (T.mul a c) (T.mul b d),
                                               T.add (T.mul b c) (T.mul a d))
  fun div ((a,b): complex) ((c,d): complex) =
    let (x,y) = mul (a,b) (c,d)
    in (T.div x (T.add (T.mul c c) (T.mul d d)),
        T.div y (T.add (T.mul c c) (T.mul d d)))

  fun sqrt ((a,b): complex) =
    let gamma = T.sqrt (T.div
                        (T.add a (T.sqrt (T.add (T.mul a a) (T.mul b b))))
                        (T.fromInt 2))
    let delta = T.mul (T.fromInt (T.sgn b))
                      (T.sqrt (T.div (T.add (T.sub (T.fromInt 0) a)
                                      (T.sqrt (T.add (T.mul a a) (T.mul b b))))
                               (T.fromInt 2)))
    in (gamma, delta)
}
