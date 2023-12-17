def fun [k] (n:i64) (op:[k]f32 -> *[k]f32) (_:[k]f32) =
  let process (Q:*[n+1][k]f32) i =
    let q = op Q[i]
    in Q with [i+1] = q
  let Q = replicate (n+1) (replicate k 0)
  let Q = process Q 0
  in Q

entry test n k = fun n copy (replicate k 0)
