-- ==
-- input {
--   [1,2]
--   [4,5,6]
-- }
-- output {
--   [2,3,5,6,7]
-- }
fun [int] main([int] a, [int] b) =
  concatMap(fn [int] ([int] r) =>
              map(+1, r),
            a, b)
