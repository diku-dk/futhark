type t = i32
type ts = []t

let main(xs: ts, x: t): ts =
  map (+x) xs
