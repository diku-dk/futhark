fun main(xs:[n][n][n][n][n]i32) =
  map (fn (i:i32) =>
    map (fn (j:i32) =>
      map (fn (k:i32) =>
        map (fn (m:i32) =>
          map (fn (n:i32) =>
            xs[i,j,k,m,n] + xs[j,k,m,n,i] + xs[k,m,n,i,j] + xs[m,n,i,j,k]
          ) (iota(n))
        ) (iota(n))
      ) (iota(n))
    ) (iota(n))
  ) (iota(n))
