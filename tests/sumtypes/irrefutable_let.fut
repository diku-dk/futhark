-- Irrefutable pattern in let-binding.

type t = #x i32

entry main (x: i32) =
  let #x y = #x x : t
  in y
