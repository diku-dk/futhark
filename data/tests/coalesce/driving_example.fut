fun main(muls:[k]i32, arr: [n][m]i32):[][][]i32 =
  map (fn (i:i32):[n][m]i32 =>
    map (fn (j:i32):[m]i32 => 
      map (fn (k:i32):i32 =>
        let mul = muls[k] in
        mul * arr[i,j] 
      ) (iota(k))
    ) (iota(m))
  ) (iota(n))
