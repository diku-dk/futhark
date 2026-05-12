-- Reduce with a fancier operator.
-- ==
-- tags { autodiff }
-- entry: rev
-- input { [1.0,2.0,3.0] [2.0,3.0,4.0] [3.0,4.0,5.0] [4.0,5.0,6.0] }
-- output { [47.0, 28.0, 32.0]
--          [83.0, 44.0, 32.0]
--          [47.0, 42.0, 42.0]
--          [83.0, 66.0, 42.0] }

def mm2by2 (a1: f64, b1: f64, c1: f64, d1: f64)
           (a2: f64, b2: f64, c2: f64, d2: f64) =
  ( a1 * a2 + b1 * c2
  , a1 * b2 + b1 * d2
  , c1 * a2 + d1 * c2
  , c1 * b2 + d1 * d2
  )

def red_mm [n] (xs: [n](f64, f64, f64, f64)) =
  reduce mm2by2 (1, 0, 0, 1) xs

entry rev [n] (xs1: [n]f64) (xs2: [n]f64) (xs3: [n]f64) (xs4: [n]f64) =
  vjp red_mm (zip4 xs1 xs2 xs3 xs4) (1, 1, 1, 1)
  |> unzip4
