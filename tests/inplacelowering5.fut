-- Based on issue #1460
-- ==
-- input { 2i64 2i64 }
-- output {
-- [[[0i64, 0i64], [0i64, 1i64]],
--  [[0i64, 0i64], [0i64, 1i64]],
--  [[0i64, 0i64], [0i64, 1i64]],
--  [[0i64, 0i64], [0i64, 1i64]],
--  [[0i64, 0i64], [0i64, 1i64]],
--  [[0i64, 0i64], [0i64, 1i64]],
--  [[0i64, 0i64], [0i64, 1i64]],
--  [[0i64, 0i64], [0i64, 1i64]],
--  [[0i64, 0i64], [0i64, 1i64]],
--  [[0i64, 0i64], [0i64, 1i64]]]
-- }

def update [n] (mat: *[n][n]i64): *[n][n]i64 =
    let mat = transpose mat
    in mat with [n-1] = iota n

def run (t:i64) (n:i64): [n][n]i64 =
    let mat = tabulate_2d n n (+) in
    loop (mat) for i < t do update mat

def main (t:i64) (n:i64) =
    map (\i -> run t n) (iota 10)
