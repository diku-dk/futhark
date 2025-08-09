-- ==
-- error: Sizes.*do not match

type square [n] 't = [n][n]t

def ext_square n : square [] i64 = tabulate_2d (n + 1) (n + 2) (\i j -> i + j)
