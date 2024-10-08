-- f32 test.  Does not test for infinity/NaN as we have no way of writing
-- that in Futhark yet.  Does test for overflow.
entry add = map2 f32.(+)

entry sub = map2 f32.(-)

entry mul = map2 f32.(*)

entry div = map2 f32.(/)

entry mod = map2 f32.(%)

entry pow = map2 f32.(**)-- ==
-- entry: add
-- input  { [0.0f32, 1.0f32, 1.0f32, -1.0f32, 3.402823e38f32, 0f32,    0f32]
--          [0.0f32, 0.0f32, 0.0f32,  0.0f32, 10f32,          f32.nan, f32.inf] }
-- output { [0.0f32, 1.0f32, 1.0f32, -1.0f32, 340282306073709652508363335590014353408.000000f32, f32.nan, f32.inf] }
-- ==
-- entry: sub
-- input  { [0.0f32, 0.0f32, 0.0f32, -3.402823e38f32]
--          [0.0f32, 1.0f32, -1.0f32, 10f32] }
-- output { [0.0f32, -1.0f32, 1.0f32, -340282306073709652508363335590014353408.000000f32] }
-- ==
-- entry: mul
-- input  { [0.0f32, 0.0f32,  0.0f32,  1.0f32, 2.0f32]
--          [0.0f32, 1.0f32, -1.0f32, -1.0f32, 1.5f32] }
-- output { [0.0f32, 0.0f32, 0.0f32, -1.0f32, 3.0f32] }
-- ==
-- entry: div
-- input { [0.0f32, 0.0f32, 1.0f32, 2.0f32]
--         [1.0f32, -1.0f32, -1.0f32, 1.5f32] }
-- output { [0.0f32, 0.0f32, -1.0f32, 1.3333333333333f32] }
-- ==
-- entry: mod
-- input { [0.0f32,  0.0f32,  1.0f32, 2.0f32]
--         [1.0f32, -1.0f32, -1.0f32, 1.5f32] }
-- output { [0.0f32, -0.0f32, -0.0f32, 0.5f32] }
-- ==
-- entry: pow
-- input  { [0.0f32,  1.0f32, 2.0f32,                2.0f32]
--          [1.0f32, -1.0f32, 1.5f32,                0f32] }
-- output { [0.0f32,  1.0f32, 2.8284271247461903f32, 1f32] }
