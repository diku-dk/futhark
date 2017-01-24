fun main(xs:[m][n]i32):[][][]i32 =
  map (fn (k:i32):[][]i32 =>
    map (fn (i:i32):[]i32 =>
      map (fn (j:i32):i32 =>
        let a = xs[i,j]
        let b = xs[j,i]
        in a+b+k
      ) (iota(n))
    ) (iota(n))
  ) (iota(10))
