import "../../accs/intrinsics"

-- ==
-- tags { autodiff }

def f (acc: *acc ([]i32)) i = write acc i (i32.i64 i)

-- ==
-- entry: prim
-- input { [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] }
-- output { [0, 2, 4, 6, 8, 10, 12, 14, 16, 18] }

entry prim [n] (xs: [n]i32) =
  let (xs': *[n]i32) = copy xs
  in reduce_by_index_stream xs' (+) 0 f (map i64.i32 (xs :> [n]i32))

-- ==
-- entry: f_jvp f_jmp
-- input { [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] }
-- output {
-- [[2, 2, 2, 2, 2, 2, 2, 2, 2, 2],
--  [2, 0, 0, 0, 0, 0, 0, 0, 0, 0]]
-- }

def seeds (n: i64) = [replicate n 1i32, replicate n 0i32 with [0] = 1]

entry f_jvp [n] (xs: *[n]i32) = map (jvp prim xs) (seeds n)

entry f_jmp [n] (xs: *[n]i32) = jmp prim xs (seeds n)
