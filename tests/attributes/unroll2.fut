-- ==
-- input { [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] }
-- output { 47 }
-- structure { Loop 0 }

def main (xs: [10]i32) =
  #[unroll]
  loop sum = 2 for x in xs do sum + x
