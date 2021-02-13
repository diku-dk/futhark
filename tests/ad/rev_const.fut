-- What happens if a result is constant?
-- ==
-- compiled input { 1i32 2i32 } output { 1i32 1i32 }

let main (x: f32) (y: f32) =
  vjp (\(x',y') -> (x' + y', 0)) (x,y) (1, 0)
