-- What happens if a result is constant?
-- ==
-- input { 1f32 2f32 } output { 1f32 1f32 }

def main (x: f32) (y: f32) =
  vjp (\(x', y') -> (x' + y', 0)) (x, y) (1, 0)
