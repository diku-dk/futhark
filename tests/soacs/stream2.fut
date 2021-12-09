-- A stream reduction where the chunks must be consecutive
-- subsequences of the original input.
-- ==
-- compiled input { 10000i64 }   output { 49995000i32 }
-- compiled input { 100000i64 }  output { 704982704i32 }
-- compiled input { 1000000i64 } output { 1783293664i32 }

-- This is just a fancy way of summing iota.
def main (n: i64) =
  let sumup k (chunk: [k]i32): i32 =
    if k == 0 then 0
    else let j = chunk[0]
         in loop x = 0 for i < k do x + i32.i64 i + j
  in reduce_stream (+) sumup (map i32.i64 (iota n))
