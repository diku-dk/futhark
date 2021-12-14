-- Type inference for infix operators.
-- ==
-- input { 1f32 0f32 } output { 1f32 false true }
-- input { 1f32 2f32 } output { 3f32 true true }

def f op x y = x `op` y

def g (+) x y = x + y

def main (x: f32) (y: f32) = (f (+) x y, g (<) x y, g (<) false true)
