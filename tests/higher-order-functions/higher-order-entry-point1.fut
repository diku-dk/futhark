-- Entry point functions are not allowed to return a function.
-- ==
-- error: Entry point functions may not be higher-order

let main (x : i32) (y : i32) : i32 -> i32 =
  \(z:i32) -> x + y + z
