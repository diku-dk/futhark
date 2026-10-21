entry mkarr (n: i64) : [n]i32 = map i32.i64 (iota n)

entry mkarr2d (n: i64) : [n][2]i32 = map (\i -> [i32.i64 i, 0]) (iota n)
