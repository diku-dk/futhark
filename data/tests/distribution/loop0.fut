// When distributing this program we should interchange the outer
// parallel loop with the inner sequential one.  The loop is just a
// sequentially written reduction with zipWith(+).
//
// Expected structure:
//
// loop
//   map
//     map
//
// --
//
// input {
//   [[[1,2],[3,4],[5,6]],
//    [[1,1],[3,4],[5,6]]]
// }
// output {
//   [[9, 12], [9, 11]]
// }
//
// structure distributed { Map/DoLoop 0 }

fun [[int,k],n] main([[[int,k],m],n] a) =
  let acc = replicate(k, 0) in
  map(fn [int,k] ([[int,k],m] a_r) =>
        loop(acc) = for i < m do
          zipWith(+, acc, a_r[i]) in
        acc
     , a)
