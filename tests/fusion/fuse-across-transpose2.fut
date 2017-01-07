-- ==
-- structure { Map 2 }
fun main(a_1: [n][m]int, a_2: [n][m]int): [][]int =
  let a = map (fn (a_1r: [m]int) (a_2r: [m]int): [m](int,int)  => zip a_1r a_2r) (
                  a_1) (a_2)
  let b = map (fn (row: [](int,int)): [m](int,int)  =>
                map (fn (x: int, y: int): (int,int)  =>
                      (x+y,x-y)) row) a
  let c = map (fn (row: [](int,int)): [n]int  =>
                map (fn (x,y) => x + y) row) (
              transpose(b))
  in c
