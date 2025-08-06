-- Test f64.(fma,mad).  The test values here are very crude and do not
-- actually test the numerical properties we hope to get.
-- ==
-- input { [1f64, 2f64, 3f64 ]
--         [3f64, 2f64, 1f64 ]
--         [2f64, 3f64, 1f64 ]
--       }
-- output { [5f64, 7f64, 4f64]
--          [5f64, 7f64, 4f64]
--        }

def main (as: []f64) (bs: []f64) (cs: []f64) =
  ( map3 f64.fma as bs cs
  , map3 f64.mad as bs cs
  )
