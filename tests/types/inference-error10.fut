-- Lambda-binding freezes an otherwise general function.
-- ==
-- error: Type mismatch

let main (x: i32) (y: bool) =
  let f x y = (y,x)
  in (\g -> (g x y, g y x)) f
