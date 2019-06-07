-- A stream reduction where the chunks must be consecutive
-- subsequences of the original input.
-- ==
-- compiled input { 10000 }   output { 49995000i32 }
-- compiled input { 100000 }  output { 704982704i32 }
-- compiled input { 1000000 } output { 1783293664i32 }

-- This is just a fancy way of summing iota.
let main (n: i32) =
  let sumup k (chunk: [k]i32) =
    if k == 0 then 0
    else let j = unsafe chunk[0]
         in loop x = 0 for i < k do x + i + j
  in stream_red (+) sumup (iota n)
