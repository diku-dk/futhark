-- A let-bound function can be instantiated for different types.
-- ==

def main (x: i32) (y: bool) =
  let f 'a 'b (x: a) (y: b) = (y,x)
  in (f x y, f y x)
