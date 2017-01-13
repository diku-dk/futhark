-- ==
-- input {
--   [[1,2,3],[4,5,6],[7,8,9]]
-- }
-- output {
--   [[3, 4, 5], [7, 9, 11], [14, 17, 20]]
-- }
-- structure { Map 2 Scan 1 }
fun main(input: [][3]int): [][]int =
  let x = scan (\(a: []int) (b: []int): [3]int  ->
                 map (+) a b) (
               replicate 3 0) input in
  map (\(r: []int): [3]int  ->
        map (+2) r) x
