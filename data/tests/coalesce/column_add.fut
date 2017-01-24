fun main(matrix:[n][m]i32, add:[l]i32) =
  map (fn (k:i32):[n][m]i32 =>
    map (fn (i:i32):[m]i32 =>
      map (fn (j:i32):i32 =>
        matrix[j,i] + add[k]
      ) (iota(m))
    ) (iota(n))
  ) (iota(l))
