-- Irrefutable pattern in parameter.

type t = #x i32

def f ((#x y): t) = y

entry main (x: i32) =
  f (#x x)
