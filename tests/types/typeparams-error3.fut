-- Cannot let-bind a type parameter.
-- ==
-- error: type parameter

let main (x: i32) =
  let 't y = x
  in x