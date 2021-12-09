-- NaN and inf must work.
-- ==
-- input { 2f64 }
-- output { false false false true true true true true }
-- input { f64.nan }
-- output { false false false false false false true false }
-- input { f64.inf }
-- output { false false false false true false true true }
-- input { -f64.inf }
-- output { false false false true true false true false }

def main(x: f64) =
  (x == f64.nan,
   x < f64.nan,
   x <= f64.nan,
   x < f64.inf,
   x <= f64.inf,
   x - f64.inf < x + f64.inf,
   f64.isnan (x + f64.nan),
   f64.isinf (x + f64.inf))
