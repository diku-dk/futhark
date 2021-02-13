-- What happens if not all the parameters are used?
-- ==
-- compiled input { 1i32 2i32 } output { 1i32 0i32 }

let main (x: f32) (y: f32) =
  vjp (\(x',_) -> x' + 2) (x,y) 1
