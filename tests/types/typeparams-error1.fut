-- Missing parameter to a multi-parameter parametric tpye.
-- ==
-- error: pair

type pair 'a 'b = (a,b)

let main (x: i32) (y: f64): pair f64 = (y,x)
