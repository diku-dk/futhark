-- ==
-- entry: rev fwd
-- input { [true] [[1.0,2.0,3.0]] [[0.0,1.0,0.0]] }
-- output { [[0.000000f64, 1.000000f64, 0.000000f64]] }

def f b (xs: []f64) =
  let ys = copy xs
  in if b then ys with [0] = 0 else ys

def g bs (xss: [][]f64) =
  #[unsafe]
  map2 f bs (copy xss)

entry fwd [n] bs (xss: *[n][]f64) =
  #[unsafe]
  jvp (g bs) xss

entry rev [n] bs (xss: *[n][]f64) =
  #[unsafe]
  vjp (g bs) xss
