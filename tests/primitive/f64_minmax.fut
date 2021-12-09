-- ==
-- input { 0f64 1f64 } output { 1f64 0f64 }
-- input { 1f64 1f64 } output { 1f64 1f64 }
-- input { -1f64 1f64 } output { 1f64 -1f64 }
-- input { 1f64 -1f64 } output { 1f64 -1f64 }
-- input { f64.nan -1f64 } output { -1f64 -1f64 }
-- input { -1f64 f64.nan } output { -1f64 -1f64 }
-- input { f64.nan f64.nan } output { f64.nan f64.nan }
-- input { -1f64 f64.inf } output { f64.inf -1f64 }
-- input { -1f64 -f64.inf } output { -1f64 -f64.inf }

def main(x: f64) (y: f64): (f64,f64) =
  (f64.max x y,
   f64.min x y)
