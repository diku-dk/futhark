-- Complex case; simplify away the loops.
-- ==
-- input { 10 2i64 [1,2,3] }
-- output { [1,2] }
-- structure { Loop 0 }

def main (n: i32) (a: i64) (arr: []i32) =
  #[unsafe] -- Just to make the IR cleaner.
  loop x = take a arr for _i < n do
    loop _y = take (length x) arr for _j < n do
      take a arr
