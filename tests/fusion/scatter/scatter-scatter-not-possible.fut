def main (xs: *[]i32) (ys: *[]i32) =
  let xs' = scatter xs [0] [1]
  in (xs', scatter ys [0] xs')
