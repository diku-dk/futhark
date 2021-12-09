-- ==
-- input { 8i64 }
-- output { [7, 0, 1, 2, 3, 4, 5, 6] }

def main(i: i64): []i32 =
  let a = 0..1..<i32.i64 i
  in rotate (-1) a
