-- Problem here is that we need will distribute the map
-- let arrs = map (\x -> iota(2*x)) xs
-- let arr's = map (\x arr -> reshape( (x,2), arr) $ zip xs arrs
-- let res = map(\arr' -> reduce(op(+), 0, arr')) arr's
-- ==
-- input {
--   [ 1i64, 2i64, 3i64, 4i64]
-- }
-- output {
--   [1i64, 6i64, 15i64, 28i64]
-- }
def main (xs: []i64) : []i64 =
  map (\(x: i64) ->
         let arr = #[unsafe] 0..<(2 * x)
         let arr' = #[unsafe] unflatten arr
         in reduce (+) 0 (arr'[0]) + reduce (+) 0 (arr'[1]))
      xs
