let I_mult (n: i32) (x: i32) (a: i32) : [n][n]i32 =
  let elem i j = i32.bool(i == j) *
                 (if i == x then a else 1)
  in tabulate_2d n n elem
