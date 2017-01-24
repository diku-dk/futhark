fun main(ys:[n][n]i32, xs:[n][n][n]i32) =
  map (fn (i:i32) =>
    map (fn (j:i32) =>
      map (fn (k:i32) =>
        xs[k,j,i] + ys[j,0] + ys[0,j]
      ) (iota(n))
    ) (iota(n))
  ) (iota(n))
