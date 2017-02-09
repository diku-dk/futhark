fun main(xs: [m][n]f64, muls:[k][n]f64, bias:[k][n][m]f64):[][][]f64 =
  map (\(k:i32):[n][m]f64 ->
    map (\(i:i32):[m]f64 ->
      map (\(j:i32):f64 -> 
          xs[j,i] * muls[k,i] + bias[k,i,j]
      ) (iota(m))
    ) (iota(n))
  ) (iota(k))
