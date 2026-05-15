-- | ignore

import "bucket_sort"

local
def hash (x: i32) : i32 =
  let x = u32.i32 x
  let x = ((x >> 16) ^ x) * 0x45d9f3b
  let x = ((x >> 16) ^ x) * 0x45d9f3b
  let x = ((x >> 16) ^ x)
  in i32.u32 x

local
entry mk_input (num_buckets: i64) (n: i64) =
  let xs =
    iota n
    |> map i32.i64
    |> map hash
    |> map i64.i32
    |> map (% num_buckets)
  in (num_buckets, xs)

-- ==
-- entry: is_sorted
-- "num_buckets=8"   script input { mk_input 8i64   10000i64 }
-- output { true }
-- "num_buckets=32"  script input { mk_input 32i64  10000i64 }
-- output { true }
-- "num_buckets=512" script input { mk_input 512i64 10000i64 }
-- output { true }

entry is_sorted (num_buckets: i64, xs: []i64) : bool =
  let result = bucket_sort num_buckets id xs
  let n = length result
  in tabulate n (\i -> i == 0 || result[i - 1] <= result[i]) |> and
