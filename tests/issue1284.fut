def one_scatter (n: i64) (m: i64) : [n][m]i32 =
  let res = tabulate_2d n m (\i j -> 0)
  in scatter_2d res [(0, 0)] [1]

entry foo = one_scatter 2 2

def another_scatter [n] (inp: *[n][n]i32) : *[n][n]i32 =
  scatter_2d inp [(0, 1)] [2]

entry bar = another_scatter (one_scatter 2 2)

-- ==
-- entry: foo
-- input {} output { [[1, 0], [0, 0]] }

-- ==
-- entry: bar
-- input {} output { [[1, 2], [0, 0]] }
