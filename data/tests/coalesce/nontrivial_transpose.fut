fun main(xs:[n][n]i32) =
  map (fn (i:i32) =>
    map (fn (j:i32) =>
      map (fn (k:i32) =>
        xs[i,k] + xs[j,k] + xs[k,j]
      ) (iota(n))
    ) (iota(n))
  ) (iota(n))
