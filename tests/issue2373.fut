-- ==
-- entry: update_2d
-- input { [[0, 0, 0], [0, 0, 0], [0, 0, 0]] 1 1 1 }
-- output { [[0, 0, 0], [0, 1, 0], [0, 0, 0]] }
entry update_2d [n] [m] (A: *[n][m]i32) (i: i32) (j: i32) (v: i32): [n][m]i32 = A with [i,j] = v

def v2d = [[0, 0, 0], [0, 0, 0], [0, 0, 0]]

-- ==
-- entry: main
-- input { }
-- output { [[0, 0, 0], [0, 1, 0], [0, 0, 0]] }
entry main = update_2d (copy v2d) 1 1 1
