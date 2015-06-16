// --
// input {
//   5
//   [3,9,4,7,19]
// }
// output {
//   [9, 7, 19]
//
// }
fun [int] main(int m, *[int,n] A) =
  streamMap( fn [int] (int chunk, *[int] C) =>
                    let W = filter( >6, C ) in
                    W
        , A
        )


