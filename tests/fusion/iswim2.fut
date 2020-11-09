-- ==
-- input {
--   [[1,2,3],[4,5,6],[7,8,9]]
--   [[4,5,6],[7,8,9],[1,2,3]]
-- }
-- output {
--   [[5, 7, 9], [16, 20, 24], [24, 30, 36]]
-- }
-- structure { /Screma 1 }
let main(input1: [][]i32) (input2: [][]i32): [][]i32 =
  let input = map(\(r1: []i32, r2: []i32)  ->
                    zip r1 r2) (zip input1 input2)
  let x = scan(\(a: [](i32,i32)) (b: [](i32,i32))  ->
                 let (a1, a2) = unzip(a)
                 let (b1, b2) = unzip(b) in
                 map(\(quad: (i32,i32,i32,i32)): (i32,i32)  ->
                       let (a1x,a2x,b1x,b2x) = quad in
                       (a1x+b1x,a2x+b2x))
                     (zip4 a1 a2 b1 b2))
               (zip (replicate 3 0) (replicate 3 0)) input in
  map (\(r: [](i32,i32)) -> map (\(x,y) -> x+y) r) x
