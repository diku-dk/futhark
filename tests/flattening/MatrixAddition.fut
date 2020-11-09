-- ==
-- input {
--   [[1,2,3],[1,2,3]]
--   [[3,2,1],[6,7,8]]
-- }
-- output {
--   [[4,4,4],[7,9,11]]
-- }
let addRows [n] (xs: [n]i32, ys: [n]i32): [n]i32 =
  map2 (+) xs ys

let addMatricies (a: [][]i32, b: [][]i32): [][]i32 =
  map  addRows (zip  a b)

let main (a: [][]i32) (b: [][]i32): [][]i32 =
  addMatricies(a,b)
