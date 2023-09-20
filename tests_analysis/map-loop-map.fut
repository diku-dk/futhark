entry main [m][n][o] (xsss: [m][n][o]i64) : [m]i64 =
  map ( \x ->
      loop s = 0 for i < o
      do
        reduce (+) s <| map ( \y ->
           i + xsss[x][y][i]
        ) (iota n)

  ) <| iota m
