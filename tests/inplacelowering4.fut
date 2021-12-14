-- Based on issue #1460
-- ==
-- input { 2i64 2i64 2i64 }
-- output {
-- [[[0i64, 1i64], [0i64, 1i64]],
--  [[0i64, 1i64], [0i64, 1i64]],
--  [[0i64, 1i64], [0i64, 1i64]],
--  [[0i64, 1i64], [0i64, 1i64]],
--  [[0i64, 1i64], [0i64, 1i64]],
--  [[0i64, 1i64], [0i64, 1i64]],
--  [[0i64, 1i64], [0i64, 1i64]],
--  [[0i64, 1i64], [0i64, 1i64]],
--  [[0i64, 1i64], [0i64, 1i64]],
--  [[0i64, 1i64], [0i64, 1i64]]]
-- }

def update [n] [m] (mat: *[n][m]i64): *[n][m]i64 =
    let mat = rotate (-1) mat
    in mat with [n-1] = iota m

def run (t:i64) (n:i64) (m:i64): [n][m]i64 =
    let mat = tabulate_2d n m (+) in
    loop (mat) for i < t do update mat

def main (t:i64) (n:i64) (m:i64) =
    map (\i -> run t n m) (iota 10)
