fun main(muls:[]i32, arr: [n][m]i32):[][]i32 =
  map (fn (i:i32):[m]i32 =>
    map (fn (j:i32):i32 => 
      let mul = muls[i*m + j] in
      mul * arr[i,j] 
    ) (iota(m))
  ) (iota(n))
