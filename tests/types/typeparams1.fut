-- Multi-parameter parametric type.
-- ==
-- input { 1 2.0f32 } output { 2.0f32 1 }

type pair 'a 'b = (a, b)

def main (x: i32) (y: f32) = (y, x) : pair f32 i32
