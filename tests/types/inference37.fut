def I_mult (n: i64) (x: i64) (a: i64) : [n][n]i64 =
  let elem i j =
    i64.bool (i == j)
    * (if i == x then a else 1)
  in tabulate_2d n n elem
