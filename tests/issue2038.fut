entry all_work_indices (n:i64) (m:i64) =
  let block i prog prog' =
    (i, prog, i64.max 0 (i64.min (n-i) (if i == 0 then n else prog')))
  let size (_, a, b) = b-a >> 1
  let (iter, _) = loop (iter, progress) = (0, replicate m 0)
                  while not (all id (tabulate m (\i -> progress[i] >= n-i-1))) do
                  let blockrow = map3 block
                                      (iota m) (progress) (rotate (-1) progress)
                  let sizes = map size blockrow
                  in (iter + 1i32, map2 (+) progress sizes)
  in iter
