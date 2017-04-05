-- ==
-- input { [[1,2,3],[4,5,6]] [[7,8,9],[1,2,3]] }
-- output { [[10, 7], [12, 9], [14, 11]] }
-- structure { Map 2 }
let main(a: [#n][#m]i32, b: [#n][#m]i32): [][]i32 =
  let a2 = map (\r: [n]i32  -> map (+1) r) (transpose(a))
  let b2 = map (\r: [n]i32  -> map (+1) r) (transpose(b))
  let c  = map (\rp: [n]i32  ->
                 let (rx,ry) = rp in
                 map (+) rx ry)
                 (zip a2 b2) in
  c
