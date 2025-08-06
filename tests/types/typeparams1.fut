-- Multi-parameter parametric type.
-- ==
-- input { 1 2.0 } output { 2.0 1 }

type pair 'a 'b = (a, b)

def main (x: i32) (y: f64) = (y, x) : pair f64 i32
