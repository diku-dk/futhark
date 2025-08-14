-- Test f32.(fma,mad).  The test values here are very crude and do not
-- actually test the numerical properties we hope to get.
-- ==
-- input { [1f32, 2f32, 3f32 ]
--         [3f32, 2f32, 1f32 ]
--         [2f32, 3f32, 1f32 ]
--       }
-- output { [5f32, 7f32, 4f32]
--          [5f32, 7f32, 4f32]
--        }

def main (as: []f32) (bs: []f32) (cs: []f32) =
  ( map3 f32.fma as bs cs
  , map3 f32.mad as bs cs
  )
