-- Problem here is that we need will distribute the map
-- let arrs = map (\x -> iota(2*x)) xs
-- let arr's = map (\x arr -> reshape( (x,2), arr) $ zip xs arrs
-- let res = map(\arr' -> reduce(op(+), 0, arr')) arr's
-- ==
-- input {
--   [ 1, 2, 3, 4]
-- }
-- output {
--   [1, 6, 15, 28]
-- }
fun main (xs: []i32): []i32 =
  map (\(x: i32): i32  ->
        let arr = iota(2 * x)
        let arr' = reshape (2,x) arr in
            reduce (+) 0 (arr'[0]) + reduce (+) 0 (arr'[1])
     ) xs
