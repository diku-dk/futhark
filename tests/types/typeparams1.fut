-- Multi-parameter parametric type.
-- ==
-- input { 1 2.0 } output { 2.0 1 }

type pair 'a 'b = (a,b)

let main (x: i32) (y: f64): pair f64 i32 = (y,x)
