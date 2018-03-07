-- We cannot map a function that returns a function over an array,
-- since that would result in an array of functions.
-- ==
-- error: Cannot .* array with elements of type .* -> .*

let main (xs : []i32) =
  map (\(x:i32) -> \(y:i32) -> x+y) xs
