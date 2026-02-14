-- ==
-- input { [1i64,2i64,3i64,4i64,5i64] [-5i64,7i64] [2i64,3i64] [3i64,4i64] }
-- output { [-2i64, 11i64] }

def main A = map3 (\x i j -> i64.sum (map (+x) A[i:j]))
