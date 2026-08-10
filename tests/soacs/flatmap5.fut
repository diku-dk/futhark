-- Like flatmap4 (a flatmap whose lambda contains a flatmap), but the inner
-- flatmap produces genuinely computed arrays (via map/iota) rather than
-- replicating, so the nested data is not merely a replicated representation.
-- ==
-- input { [0i64, 4i64, 1i64, 3i64] [0i32, 1i32, 2i32, 3i32] }
-- output { [0i64, 4i64, 1i64, 3i64]
--          [true, false, false, false, true, true, false, false]
--          [0i64, 0i64, 4i64, 5i64]
--          [true, false, false, false, true, true, false, false]
--          [10i32, 11i32, 12i32, 13i32, 20i32, 30i32, 31i32, 32i32]
--          [0i32, 1i32, 2i32, 3i32] }

def main (ks: []i64) (xs: []i32) =
  let (a, b, c, d, e) =
    flatmap (\(k, x) ->
               let (_, flags, _, r, cs) =
                 flatmap (\(kk, y) -> (map (\i -> y * 10 + i32.i64 i) (iota kk), y)) [(k, x)]
               in (sized k (zip flags r), cs[0]))
            (zip ks xs)
  in (a, b, c, map (.0) d, map (.1) d, e)
