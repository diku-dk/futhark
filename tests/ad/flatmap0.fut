-- ==
-- tags { autodiff }
-- entry: fwd_map fwd_vec rev_map rev_vec
-- input { [1i64,2i64,3i64] [4.0f32,5.0f32,6.0f32] }
-- output {
-- [[1.0f32, 0.0f32, 0.0f32, 0.0f32, 0.0f32, 0.0f32],
--  [0.0f32, 1.0f32, 1.0f32, 0.0f32, 0.0f32, 0.0f32],
--  [0.0f32, 0.0f32, 0.0f32, 1.0f32, 1.0f32, 1.0f32]]
-- [[2.0f32, 0.0f32, 0.0f32],
--  [0.0f32, 2.0f32, 0.0f32],
--  [0.0f32, 0.0f32, 2.0f32]]
-- }

def primal (ks: []i64) (xs: []f32) =
  let f (k: i64, x: f32) = (replicate k x, x * 2)
  let (_, _, _, a, b) = flatmap f (zip ks xs)
  in (take (i64.sum ks) a, b)

entry fwd_map [n] (ks: [n]i64) (xs: [n]f32) : ([][]f32, [][]f32) =
  unzip
  <| tabulate n \i ->
       jvp (primal ks) xs (replicate n 0 with [i] = 1)

entry fwd_vec [n] (ks: [n]i64) (xs: [n]f32) : ([][]f32, [][]f32) =
  let seeds = tabulate n (\i -> replicate n 0 with [i] = 1)
  in unzip <| jmp (primal ks) xs seeds

entry rev_map [n] (ks: [n]i64) (xs: [n]f32) =
  unzip
  <| map split
     <| transpose
        <| tabulate (i64.sum ks + n) \i ->
             vjp (primal ks) xs ( if i < i64.sum ks
                                  then replicate (i64.sum ks) 0 with [i] = 1
                                  else replicate (i64.sum ks) 0
                                , if i < i64.sum ks
                                  then replicate n 0
                                  else replicate n 0 with [i - i64.sum ks] = 1
                                )

entry rev_vec [n] (ks: [n]i64) (xs: [n]f32) =
  let seeds =
    tabulate (i64.sum ks + n) \i ->
      ( if i < i64.sum ks
        then replicate (i64.sum ks) 0 with [i] = 1
        else replicate (i64.sum ks) 0
      , if i < i64.sum ks
        then replicate n 0
        else replicate n 0 with [i - i64.sum ks] = 1
      )
  in unzip
     <| map split
        <| transpose
           <| mjp (primal ks) xs seeds
