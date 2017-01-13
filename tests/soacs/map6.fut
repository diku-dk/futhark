-- ==
-- input {
--   [[1,2,3],[4,5,6]]
--   [[6,5,4],[3,2,1]]
-- }
-- output {
--   [[7, 7, 7], [7, 7, 7]]
--   [[-5, -3, -1], [1, 3, 5]]
-- }
fun inner(a: [][](int,int)): [][](int,int) =
  map (\(row: [](int,int)): [](int,int)  ->
        map (\(x: int, y: int): (int,int)  ->
              (x+y,x-y)) row) a

fun main(a1: [][]int, a2: [][]int): ([][]int, [][]int) =
  let a = map (\(p: ([]int,[]int)): [](int,int)  ->
                let (p1,p2) = p in
                zip p1 p2) (
              zip a1 a2) in
  unzip(map (\(r: [](int,int)): ([]int, []int)  ->
              unzip(r)) (
            inner(a)))
