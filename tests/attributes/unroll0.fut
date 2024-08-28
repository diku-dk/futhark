-- ==
-- input { [1,2,4,5,1,2,3,4,1,2,4,1,2] }
-- output { [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 4, 1, 2] }
-- structure { Loop 0 }

def main (xs: *[]i32) =
  #[unroll]
  loop xs for i < 10 do
    let xs[i] = i in xs
