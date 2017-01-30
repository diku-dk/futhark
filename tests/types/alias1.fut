type t = i32
type ts = []t

fun main(xs: ts, x: t): ts =
  map (+x) xs
