-- You may not consume a free variable inside of a lambda.
--
-- ==
-- error:

let consume(a: *[]i32): []i32 = a

let main(a: *[]i32): [][]i32 =
  map (\i -> consume a) (iota 10)
