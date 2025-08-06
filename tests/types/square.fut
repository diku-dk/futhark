-- Test that we can use type abbreviations to encode tricky existential situations.
-- ==
-- input { 2i64 }
-- output { [[0i64, 1i64, 2i64], [1i64, 2i64, 3i64], [2i64, 3i64, 4i64]] }

type square [n] 't = [n][n]t

def ext_square n : square [] i64 = tabulate_2d (n + 1) (n + 1) (\i j -> i + j)

def tr_square [n] 't (s: square [n] t) : square [n] t = transpose s

def main n = tr_square (ext_square n)
