-- ==
-- error: Dimensions.*do not match

type square [n] 't = [n][n]t

let ext_square : i64 -> square [] i64 = \n -> tabulate_2d (n+1) (n+2) (\i j -> i + j)
