-- An implementation of the NUMERIC signature using pairs of numbers.

include futlib.numeric

module Rational(T: INTEGRAL): NUMERIC {
type t = (T.t, T.t)

fun add ((a, b):t) ((c,d):t) =
  (T.add (T.mul a d) (T.mul b c), T.mul b d)
fun sub ((a, b):t) ((c,d):t) =
  (T.sub (T.mul a d) (T.mul b c), T.mul b d)
fun mul ((a, b):t) ((c,d):t) =
  (T.mul a c, T.mul b d)
fun div ((a, b):t) ((c,d):t) =
  (T.mul a b, T.mul d c)

fun tabs (x: T.t) = if T.lt x (T.fromInt 0)
                    then T.sub (T.fromInt 0) x
                    else x

fun lcm ((a,b): t) = T.mul (T.div (tabs a) (gcd (a,b))) (tabs a)

fun gcd ((a, b): t): T.t =
  loop ((a,b)) = while ! (T.eq a (T.fromInt 0)) do
    (T.mod b a, a)
  in b

fun comparable ((a,b): t) ((c,d): t) =
  let cm = lcm (b,d)
  in (T.mul a (T.div cm b),
      T.mul c (T.div cm d))

fun eq (x:t) (y:t) =
  let (a', c') = comparable x y
  in T.eq a' c'

fun lt (x:t) (y:t) =
  let (a', c') = comparable x y
  in T.lt a' c'

fun gt (x:t) (y:t) =
  let (a', c') = comparable x y
  in T.gt a' c'

fun fromInt (x: i32) = (T.fromInt x, T.fromInt 1)

}
