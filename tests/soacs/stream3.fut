-- A stream reduction where the chunks must be consecutive
-- subsequences of the original input.
-- ==
-- compiled input { 10000 }   output { 1i32 5001i32 10000i32}
-- compiled input { 100000 }  output { 1i32 50001i32 100000i32}
-- compiled input { 1000000 } output { 1i32 500001i32 1000000i32}
-- structure { Stream 1 }

-- Just a fancy way of incrementing iota.
let main (n: i32) =
  let f k (chunk: [k]i32) =
    let x = if k == 0 then 0 else unsafe chunk[0]
    in map (+x+1) (iota k)
  let xs = map_stream f (iota n)
  in (xs[0], xs[n/2], xs[n-1])
