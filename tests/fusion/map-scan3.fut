-- Mapping with a scanomap - segmented scanomap, but one that uses a
-- distinct per-segment value in the fold part.
--
-- The program is somewhat contrived to support large amount of work
-- with only small input data sets.
--
-- ==
-- input { 3i64 3i64 }
-- output { 488i32 }
-- input { 10i64 1000i64 }
-- output { 1986778316i32 }
-- compiled input { 10i64 10000i64 }
-- output { -1772567048i32 }
-- compiled input { 10000i64 10i64 }
-- output { 1666665i32 }
-- compiled input { 100000i64 10i64 }
-- output { 16511385i32 }
--
-- structure {
--   /Screma/Stream 1
--   /Screma 1
-- }
def main (n: i64) (m: i64) : i32 =
  let factors = map (^ 123) (iota n)
  let res =
    map (\factor ->
           reduce (+) 0 (scan (+) 0 (map i32.i64 (map (* factor) (iota m)))))
        factors
  in res[n - 2]
