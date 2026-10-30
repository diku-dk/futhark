
-- ==
-- tags { autodiff }
-- entry: rev fwd
-- input { [true] [[1.0f32,2.0f32,3.0f32]] [[0.0f32,1.0f32,0.0f32]] }
-- output { [[0.000000f32, 1.000000f32, 0.000000f32]] }

def f b (xs: []f32) =
  let ys = copy xs
  in if b then ys with [0] = 0 else ys

def g bs (xss: [][]f32) =
  #[unsafe]
  map2 f bs (copy xss)

entry fwd [n] bs (xss: *[n][]f32) =
  #[unsafe]
  jvp (g bs) xss

entry rev [n] bs (xss: *[n][]f32) =
  #[unsafe]
  vjp (g bs) xss
