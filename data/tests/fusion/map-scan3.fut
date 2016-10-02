-- Mapping with a scanomap - segmented scanomap, but one that uses a
-- distinct per-segment value in the fold part.
--
-- The program is somewhat contrived to support large amount of work
-- with only small input data sets.
--
-- ==
-- input { 3 3 }
-- output { 488i32 }
-- input { 10 1000 }
-- output { 1986778316i32 }
-- input { 10 10000 }
-- output { -1772567048i32 }
-- input { 10000 10 }
-- output { 1666665i32 }
-- input { 100000 10 }
-- output { 16511385i32 }
--
-- structure {
--   /Map/Stream/Scanomap 1
-- }
fun main(n: int, m: int): int =
  let factors = map (^123) (iota n)
  let res = map (fn factor =>
                   reduce (+) 0 (scan (+) 0 (map (*factor) (iota m))))
                 factors
  in res[n-2]
