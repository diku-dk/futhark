-- Test f16.(fma,mad).  The test values here are very crude and do not
-- actually test the numerical properties we hope to get.
-- ==
-- input { [1f16, 2f16, 3f16 ]
--         [3f16, 2f16, 1f16 ]
--         [2f16, 3f16, 1f16 ]
--       }
-- output { [5f16, 7f16, 4f16]
--          [5f16, 7f16, 4f16]
--        }

def main (as: []f16) (bs: []f16) (cs: []f16) =
  ( map3 f16.fma as bs cs
  , map3 f16.mad as bs cs
  )
