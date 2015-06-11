// --
// input {
//   {3, 27000000}
// }
// output {
//   [9000000.0,9000000.0,9000000.0]
// }
fun *[real,n_histo] main(int n_histo, int n_image) =
  let A = iota(n_image) in
  stream( fn *[real] (*[real] acc, [int] a) =>
                loop (acc) = for i < chunk do
                    let ind = a[i] % n_histo      in
                    let acc[ind] = acc[ind] + 1.0 in
                    acc
                in  acc
        , chunk, i, copy(replicate(n_histo,0.0)), A
        )
