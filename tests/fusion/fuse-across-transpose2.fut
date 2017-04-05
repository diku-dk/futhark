-- ==
-- structure { Map 2 }
let main(a_1: [#n][#m]i32, a_2: [#n][#m]i32): [][]i32 =
  let a = map (\(a_1r: [#m]i32) (a_2r: [#m]i32): [m](i32,i32)  -> zip a_1r a_2r) (
                  a_1) (a_2)
  let b = map (\(row: [](i32,i32)): [m](i32,i32)  ->
                map (\(x: i32, y: i32): (i32,i32)  ->
                      (x+y,x-y)) row) a
  let c = map (\(row: [](i32,i32)): [n]i32  ->
                map (\(x,y) -> x + y) row) (
              transpose(b))
  in c
