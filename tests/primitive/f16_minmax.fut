-- ==
-- input { 0f16 1f16 } output { 1f16 0f16 }
-- input { 1f16 1f16 } output { 1f16 1f16 }
-- input { -1f16 1f16 } output { 1f16 -1f16 }
-- input { 1f16 -1f16 } output { 1f16 -1f16 }
-- input { f16.nan -1f16 } output { -1f16 -1f16 }
-- input { -1f16 f16.nan } output { -1f16 -1f16 }
-- input { f16.nan f16.nan } output { f16.nan f16.nan }
-- input { -1f16 f16.inf } output { f16.inf -1f16 }
-- input { -1f16 -f16.inf } output { -1f16 -f16.inf }

def main(x: f16) (y: f16): (f16,f16) =
  (f16.max x y,
   f16.min x y)
