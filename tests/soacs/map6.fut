-- ==
-- input {
--   [[1,2,3],[4,5,6]]
--   [[6,5,4],[3,2,1]]
-- }
-- output {
--   [[7, 7, 7], [7, 7, 7]]
--   [[-5, -3, -1], [1, 3, 5]]
-- }
let inner(a: [][](i32,i32)): [][](i32,i32) =
  map (\(row: [](i32,i32)) ->
        map (\(x: i32, y: i32)  ->
              (x+y,x-y)) row) a

let main(a1: [][]i32) (a2: [][]i32): ([][]i32, [][]i32) =
  let a = map (\(p: ([]i32,[]i32)) ->
                let (p1,p2) = p in
                zip p1 p2) (
              zip a1 a2) in
  unzip(map (\(r: [](i32,i32)) ->
              unzip(r)) (
            inner(a)))
