-- Based on https://stackoverflow.com/questions/56376512/why-do-i-get-cannot-unify-t%e2%82%82-with-type-f32-when-compiling-and-how-do-i-sol
-- ==
-- error: "\^"

def hit_register (x: f32) (y: f32) : bool =
  ((x - 1.0) ^ 2.0 + (y - 1.0) ^ 2.0) <= 1.0
