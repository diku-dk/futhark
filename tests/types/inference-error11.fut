-- An unused polymorphic functional parameter is a problem.
-- ==
-- error: ambiguous

let main (x: i32) (y: bool) =
  let f x y = (y,x)
  in (\g -> (f x y, f y x)) f
