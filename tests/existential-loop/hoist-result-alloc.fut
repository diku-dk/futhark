-- The allocation for the result of 'step' must not be hoisted out of the
-- outer loop: its memory reaches the loop result only through the branch
-- (existential memory), and each iteration reads the previous state (in
-- that memory) while it writes the new one.
-- ==
-- input { 3i64 2i64 2i64 3i64 }
-- output { [[[2, 24], [16, 40]], [[3, 25], [17, 41]], [[6, 29], [21, 45]]] }

def step [n][p][l] (xs: [n][p][l]i32) : *[n][p][l]i32 =
  tabulate n (\t ->
                loop ys = replicate p (replicate l 0)
                for k < p do
                  ys with [k] = tabulate l (\i -> xs[t, k, i] + xs[i64.max 0 (t - 1), k, i]))

def main (n: i64) (p: i64) (l: i64) (steps: i64) =
  loop xs = tabulate_3d n p l (\t k i -> i32.i64 (t + 2 * k + 3 * i))
  for s < steps do
    let ys = step xs
    in if s + 1 == steps then ys else ys with [0, 0, 0] = 1
