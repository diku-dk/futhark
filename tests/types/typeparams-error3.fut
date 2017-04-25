-- Cannot let-bind a type parameter.
-- ==
-- error: Type parameter

let main (x: i32) =
  let 't y = x
  in x
