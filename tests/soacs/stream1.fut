-- A simple streamRed that does not permit interchange of the
-- reduction operator.
-- ==
-- input { [[1,2,3], [4,5,6], [6,7,8]] }
-- output { [11i32, 14i32, 17i32] }

fun vecadd(xs: *[m]i32) (ys: [m]i32): *[m]i32 =
  loop (xs) = for i < m do
    let xs[i] = xs[i] + ys[i]
    in xs
  in xs

fun process_chunk (chunk: [chunk_sz][m]i32): *[m]i32 =
  loop (acc = replicate m 0) = for i < chunk_sz do
                   vecadd acc chunk[i]
  in acc

fun main(xss: [n][m]i32): [m]i32 =
  streamRedPer vecadd process_chunk xss
