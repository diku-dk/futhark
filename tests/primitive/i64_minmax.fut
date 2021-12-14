-- ==
-- input { 0i64 1i64 } output { 1i64 0i64 }
-- input { 1i64 1i64 } output { 1i64 1i64 }
-- input { -1i64 1i64 } output { 1i64 -1i64 }
-- input { 1i64 -1i64 } output { 1i64 -1i64 }


def main(x: i64) (y: i64): (i64,i64) =
  (i64.max x y,
   i64.min x y)
