-- ==
-- structure { /Screma 1 }
def main (a_1: [][]i32, a_2: [][]i32) : [][]i32 =
  let a =
    map2 (\(a_1r: []i32) (a_2r: []i32) -> zip a_1r a_2r)
         (a_1)
         (a_2)
  let b =
    map (\(row: [](i32, i32)) ->
           map (\(x: i32, y: i32) : (i32, i32) ->
                  (x + y, x - y))
               row)
        a
  let c =
    map (\(row: [](i32, i32)) ->
           map (\(x, y) -> x + y) row)
        (transpose b)
  in c
