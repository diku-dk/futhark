-- ==
-- input { 1i64 [1,2] }
-- output { 1i64 }
-- compiled input { 1i64 [1,2,3] }
-- error: invalid size

entry main (x: i64) (_: [x + 1]i32) = x
