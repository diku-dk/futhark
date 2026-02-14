-- ==
-- input { [[[0i64,1i64],[4i64,5i64]],[[-2i64,9i64],[9i64,2i64]]] [0i64,1i64] [1i64,0i64] }
-- output { [6i64,11i64] }

def main = map3 (\A (i:i64) (j: i64) -> i64.sum (flatten A[i:,j:]))
