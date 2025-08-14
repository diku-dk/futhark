import "../../accs/intrinsics"

def f (acc: *acc ([]i32)) i = write acc i (i32.i64 i)

-- ==
-- entry: prim
-- input { [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] }
-- output { [0, 2, 4, 6, 8, 10, 12, 14, 16, 18] }

entry prim [n] (xs: [n]i32) =
  let (xs': *[n]i32) = copy xs
  in reduce_by_index_stream xs' (+) 0 f (map i64.i32 (xs :> [n]i32))

-- ==
-- entry: f_jvp
-- input { [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] }
-- output { [2, 2, 2, 2, 2, 2, 2, 2, 2, 2] }
entry f_jvp (xs: *[]i32) =
  jvp prim xs (replicate 10 1)
