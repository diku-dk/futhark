-- A let-bound function can be instantiated for different types.
-- ==

let main (x: i32) (y: bool) =
  let f x y = (y,x)
  in (f x y, f y x)
