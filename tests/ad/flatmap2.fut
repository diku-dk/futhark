-- Where the flatmap lambda has a free variable whose contributions from each
-- iteration must be summed. The total size of the data array is passed in
-- explicitly, so that the cotangent can be given a known size.
-- ==
-- tags { autodiff no_webgpu }
-- entry: fwd_map fwd_vec rev_map rev_vec
-- input { [1i64,2i64,3i64] [4.0f32,5.0f32,6.0f32] }
-- output {
-- [4.0f32, 5.0f32, 5.0f32, 6.0f32, 6.0f32, 6.0f32, 8.0f32, 10.0f32, 12.0f32]
-- [[2.0f32, 0.0f32, 0.0f32],
--  [0.0f32, 2.0f32, 0.0f32],
--  [0.0f32, 2.0f32, 0.0f32],
--  [0.0f32, 0.0f32, 2.0f32],
--  [0.0f32, 0.0f32, 2.0f32],
--  [0.0f32, 0.0f32, 2.0f32],
--  [4.0f32, 0.0f32, 0.0f32],
--  [0.0f32, 4.0f32, 0.0f32],
--  [0.0f32, 0.0f32, 4.0f32]]
-- }

def primal [n] (ks: [n]i64) (m: i64) (c: f32) (xs: [n]f32) : ([m]f32, [n]f32) =
  let f (k: i64, x: f32) = (replicate k (x * c), x * c * 2)
  let (_, _, _, a, b) = flatmap f (zip ks xs)
  in (take m a, b)

-- The function we differentiate, taking the free variable and the array as a
-- single argument.
def primal_uncurried [n] (ks: [n]i64) (m: i64) (cx: (f32, [n]f32)) : ([m]f32, [n]f32) =
  primal ks m cx.0 cx.1

-- One tangent per component of the differentiated argument, which is the pair
-- of the free variable and the array.
def tangents (n: i64) : [1 + n](f32, [n]f32) =
  [(1, replicate n 0)] ++ tabulate n (\i -> (0, replicate n 0 with [i] = 1))

-- One cotangent per scalar of the two results.
def cotangents (n: i64) (m: i64) =
  let a_seeds = tabulate m (\i -> (replicate m 0f32 with [i] = 1, replicate n 0f32))
  let b_seeds = tabulate n (\i -> (replicate m 0f32, replicate n 0f32 with [i] = 1))
  in a_seeds ++ b_seeds

-- Turn the Jacobian columns produced by forward mode into the same shape that
-- the reverse entries produce: the derivative of every result with respect to
-- the free variable, and then with respect to the array.
def columns [n] [k] (cols: [1 + n][k]f32) : ([k]f32, [k][n]f32) =
  (cols[0], transpose (cols[1:] :> [n][k]f32))

entry fwd_map [n] (ks: [n]i64) (xs: [n]f32) : ([]f32, [][]f32) =
  let m = i64.sum ks
  in columns
     <| map (\t ->
               let (da, db) = jvp (primal_uncurried ks m) (2, xs) t
               in da ++ db)
            (tangents n)

entry fwd_vec [n] (ks: [n]i64) (xs: [n]f32) : ([]f32, [][]f32) =
  let m = i64.sum ks
  in columns
     <| map (\(da, db) -> da ++ db)
            (jmp (primal_uncurried ks m) (2, xs) (tangents n))

entry rev_map [n] (ks: [n]i64) (xs: [n]f32) : ([]f32, [][]f32) =
  let m = i64.sum ks
  in unzip
     <| map (vjp (primal_uncurried ks m) (2, xs)) (cotangents n m)

entry rev_vec [n] (ks: [n]i64) (xs: [n]f32) : ([]f32, [][]f32) =
  let m = i64.sum ks
  in unzip
     <| mjp (primal_uncurried ks m) (2, xs) (cotangents n m)
