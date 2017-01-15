-- ==
-- input {
-- }
-- output {
--   [[0, 9, 18], [27, 36, 45], [54, 63, 72]]
-- }
fun main(): [][]i32 =
  let n = 9
  let a = map (\(i: i32): []i32  ->
                replicate n i) (
              iota(n))
  let b = reshape (3,3,9) a in
  map  (\(row: [][]i32): []i32  ->
         map  (\(x: []i32): i32  -> reduce (+) 0 x) row) b
