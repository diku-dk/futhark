-- Irrefutable pattern in case.

type t = #x i32

entry main (x: i32) =
  match #x x : t
  case #x x -> x
  case _ -> 123
