fun main(xs: [m][n]i32, muls:[k][n]i32, bias:[k][n][m]i32):[][][]i32 =
  map (fn (k:i32):[n][m]i32 =>
    map (fn (i:i32):[m]i32 =>
      map (fn (j:i32):i32 => 
          xs[j,i] * muls[k,i] + bias[k,i,j]
      ) (iota(m))
    ) (iota(n))
  ) (iota(k))
