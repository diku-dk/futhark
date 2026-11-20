-- As flatmap0.fut, but where the flatmap produces multidimensional arrays -
-- both the nonuniform (concatenated) result and the uniform one.
-- ==
-- tags { autodiff no_webgpu }
-- entry: fwd_map fwd_vec rev_map rev_vec
-- input { [1i64,2i64,3i64] [4.0f32,5.0f32,6.0f32] }
-- output {
-- [[[1.0f32, 8.0f32], [0.0f32, 0.0f32], [0.0f32, 0.0f32], [0.0f32, 0.0f32], [0.0f32, 0.0f32], [0.0f32, 0.0f32]],
--  [[0.0f32, 0.0f32], [1.0f32, 10.0f32], [1.0f32, 10.0f32], [0.0f32, 0.0f32], [0.0f32, 0.0f32], [0.0f32, 0.0f32]],
--  [[0.0f32, 0.0f32], [0.0f32, 0.0f32], [0.0f32, 0.0f32], [1.0f32, 12.0f32], [1.0f32, 12.0f32], [1.0f32, 12.0f32]]]
-- [[[2.0f32, 3.0f32], [0.0f32, 0.0f32], [0.0f32, 0.0f32]],
--  [[0.0f32, 0.0f32], [2.0f32, 3.0f32], [0.0f32, 0.0f32]],
--  [[0.0f32, 0.0f32], [0.0f32, 0.0f32], [2.0f32, 3.0f32]]]
-- }

def primal (ks: []i64) (xs: []f32) =
  let f (k: i64, x: f32) = (replicate k [x, x * x], [x * 2, x * 3])
  let (_, _, _, a, b) = flatmap f (zip ks xs)
  in (take (i64.sum ks) a, b)

entry fwd_map [n] ks (xs: [n]f32) : ([][][]f32, [][][]f32) =
  unzip
  <| tabulate n \i ->
       jvp (primal ks) xs (replicate n 0 with [i] = 1)

entry fwd_vec [n] ks (xs: [n]f32) : ([][][]f32, [][][]f32) =
  let seeds = tabulate n (\i -> replicate n 0 with [i] = 1)
  in unzip <| jmp (primal ks) xs seeds

def seeds [n] (ks: [n]i64) =
  let zero_a = replicate (i64.sum ks) (replicate 2 0f32)
  let zero_b = replicate n (replicate 2 0f32)
  let a_seeds =
    flatten <| tabulate_2d (i64.sum ks) 2 \r c -> (copy zero_a with [r, c] = 1, zero_b)
  let b_seeds =
    flatten <| tabulate_2d n 2 \r c -> (zero_a, copy zero_b with [r, c] = 1)
  in (a_seeds, b_seeds)

-- Turn one Jacobian row per result scalar into one column per input.
def jacobian [k] [p] [q] (rows: [p * q][k]f32) : [k][p][q]f32 =
  map unflatten (transpose rows)

entry rev_map [n] (ks: [n]i64) (xs: [n]f32) : ([][][]f32, [][][]f32) =
  let (a_seeds, b_seeds) = seeds ks
  in ( jacobian <| map (vjp (primal ks) xs) a_seeds
     , jacobian <| map (vjp (primal ks) xs) b_seeds
     )

entry rev_vec [n] (ks: [n]i64) (xs: [n]f32) : ([][][]f32, [][][]f32) =
  let (a_seeds, b_seeds) = seeds ks
  in ( jacobian <| mjp (primal ks) xs a_seeds
     , jacobian <| mjp (primal ks) xs b_seeds
     )
