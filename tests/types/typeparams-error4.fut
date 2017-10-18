-- Cannot loop-bind a type parameter.
-- ==
-- error: Type parameter

let main (x: i32) =
  loop 't y = x for i < x do x + 1
