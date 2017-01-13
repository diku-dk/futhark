-- ==
-- structure { Map 2 }
fun main(a_1: [n][m]int, a_2: [n][m]int): [][]int =
  let a = map (\(a_1r: [m]int) (a_2r: [m]int): [m](int,int)  -> zip a_1r a_2r) (
                  a_1) (a_2)
  let b = map (\(row: [](int,int)): [m](int,int)  ->
                map (\(x: int, y: int): (int,int)  ->
                      (x+y,x-y)) row) a
  let c = map (\(row: [](int,int)): [n]int  ->
                map (\(x,y) -> x + y) row) (
              transpose(b))
  in c
