-- What happens if not all the parameters are used?
-- ==
-- input { 1f32 2f32 } output { 1f32 0f32 }

def main (x: f32) (y: f32) =
  vjp (\(x',_) -> x' + 2) (x,y) 1
