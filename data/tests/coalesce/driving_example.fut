fun main(xs: [n][m]i32, muls:[k]i32, bias:[k][m][n]i32):[][][]i32 =
  map (fn (k:i32):[n][m]i32 =>
    map (fn (i:i32):[m]i32 =>
      map (fn (j:i32):i32 => 
          xs[j,i] * muls[k] + bias[k,i,j]
      ) (iota(m))
    ) (iota(n))
  ) (iota(k))
