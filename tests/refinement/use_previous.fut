let fun1 [n] (xs: [n]i64) : {[n-1]i64 | \res-> is_indexfn res} =
  map (\i -> xs[i+1] + 1337) (iota (n-1))

let fun2 [n] (inputs: [n]i64) : {[n-1]i64 | \res-> is_indexfn res} =
  -- let zs = fun1 (replicate n 0)
  let zs = fun1 inputs
  in map (\x -> x * 2) zs

let fun3 [n] (_inputs: [n]i64) : {[n-1]i64 | \res-> is_indexfn res} =
  let zs = fun1 (replicate n 0)
  in map (\x -> x * 2) zs

let sum [n] (xs: [n]i64) : {[n]i64 | \res-> is_indexfn res} =
  scan (+) 0 xs

let fun4 [n] (inputs: [n]i64) : {[n]i64 | \res-> is_indexfn res} =
  let zs = sum inputs
  in map (\x -> x * 2) zs

let fun5 [n] (_inputs: [n]i64) : {[n]i64 | \res-> is_indexfn res} =
  let zs = sum (replicate n 1)
  in map (\x -> x * 2) zs
