def main (n: i64) m (A: [][]i64) =
  tabulate_2d  n m (\i j -> #[unsafe] A[j+1,i-1])
