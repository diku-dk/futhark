-- A simple stream_red that does not permit interchange of the
-- reduction operator.
-- ==
-- input { [[1,2,3], [4,5,6], [6,7,8]] }
-- output { [11i32, 14i32, 17i32] }

let vecadd(xs: *[#m]i32) (ys: [#m]i32): *[m]i32 =
  loop (xs) for i < m do
    let xs[i] = xs[i] + ys[i]
    in xs

let process_chunk (chunk: [#chunk_sz][#m]i32): *[m]i32 =
  loop (acc = replicate m 0) for i < chunk_sz do
                   vecadd acc chunk[i]

let main(xss: [#n][#m]i32): [m]i32 =
  stream_red_per vecadd process_chunk xss
