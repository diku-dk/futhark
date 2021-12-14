-- NaN and inf must work.
-- ==
-- input { 2f32 }
-- output { false false false true true true true true }
-- input { f32.nan }
-- output { false false false false false false true false }
-- input { f32.inf }
-- output { false false false false true false true true }
-- input { -f32.inf }
-- output { false false false true true false true false }


def main(x: f32) =
  (x == f32.nan,
   x < f32.nan,
   x <= f32.nan,
   x < f32.inf,
   x <= f32.inf,
   x - f32.inf < x + f32.inf,
   f32.isnan (x + f32.nan),
   f32.isinf (x + f32.inf))
