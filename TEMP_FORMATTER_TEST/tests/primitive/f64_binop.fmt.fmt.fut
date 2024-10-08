-- f64 test.  Does not test for infinity/NaN as we have no way of writing
-- that in Futhark yet.  Does test for overflow.
entry add = map2 f64.(+)

entry sub = map2 f64.(-)

entry mul = map2 f64.(*)

entry div = map2 f64.(/)

entry mod = map2 f64.(%)

entry pow = map2 f64.(**)-- ==
-- entry: add
-- input  { [0.0f64, 1.0f64, 1.0f64, -1.0f64, 1.79769e308, 0f64,    0f64]
--          [0.0f64, 0.0f64, 0.0f64,  0.0f64, 10f64,       f64.nan, f64.inf] }
-- output { [0.0f64, 1.0f64, 1.0f64, -1.0f64, 1.79769e308, f64.nan, f64.inf] }
-- ==
-- entry: sub
-- input  { [0.0f64, 0.0f64, 0.0f64, -1.79769e308]
--          [0.0f64, 1.0f64, -1.0f64, 10f64] }
-- output { [0.0f64, -1.0f64, 1.0f64, -1.79769e308] }
-- ==
-- entry: mul
-- input  { [0.0f64, 0.0f64,  0.0f64,  1.0f64, 2.0f64]
--          [0.0f64, 1.0f64, -1.0f64, -1.0f64, 1.5f64] }
-- output { [0.0f64, 0.0f64, 0.0f64, -1.0f64, 3.0f64] }
-- ==
-- entry: div
-- input { [0.0f64, 0.0f64, 1.0f64, 2.0f64]
--         [1.0f64, -1.0f64, -1.0f64, 1.5f64] }
-- output { [0.0f64, 0.0f64, -1.0f64, 1.3333333333333f64] }
-- ==
-- entry: mod
-- input { [0.0f64,  0.0f64,  1.0f64, 2.0f64]
--         [1.0f64, -1.0f64, -1.0f64, 1.5f64] }
-- ==
-- entry: pow
-- input  { [0.0f64,  1.0f64, 2.0f64,                2.0f64]
--          [1.0f64, -1.0f64, 1.5f64,                0f64] }
-- output { [0.0f64,  1.0f64, 2.8284271247461903f64, 1f64] }
