-- ==
-- input { 8i64 }
-- output { [1i64, 2i64, 3i64, 4i64, 5i64, 6i64, 7i64, 0i64] }

def main(i: i64): []i64 =
  let a = iota(i)
  in rotate 1 a
