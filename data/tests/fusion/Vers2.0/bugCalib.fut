// --
// input {
//    [1.0, 2.0, 3.0, 4.0, 5.0]
// }
// output {
//    [2.0, 3.0, 4.0, 5.0, 0.0]
// }
fun [real] main( [real] result ) =
  // 0 <= i < m AND 0 <= j < n
  let m = size(0, result) in
  map ( fn real (int j) =>
            if j < (m-1)
            then result[j+1]
            else 0.0
      , iota(m) )
