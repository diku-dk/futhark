// When distributing, the stream should be removed and the body
// distributed.
//
// --
// structure distributed { Map 3 Reduce 1 }

fun [int] main([[int,n]] A) =
  map(fn int ([int] A_row) =>
        streamSeq( fn int (int chunk, int acc, [int] C) =>
                     let W = filter( >6, C ) in
                     let W_sum = reduce(+, 0, W) in
                     acc+W_sum
                 , 0, A_row
                 ),
        A)
