def f (i: i64) (n: i64) : i64 =
  if i < 0 || n <= i then -1 else i

def g [m] (arr: *[m]i64) : []i64 =
  let set =
    reduce_by_index (replicate m 0i64)
                    (+)
                    0
                    (map (\k -> f k m) arr)
                    arr
  in filter (\i -> set[f i m] == 0) (iota m)

entry main (n: i64) : i64 =
  let arr =
    loop arr = iota n
    while length arr > 1 do
      g arr
  in length arr
