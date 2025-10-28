-- Based on issue 1351.
-- ==
-- input { [[1.0,2.0,3.0],[4.0,5.0,6.0]] 0i64 4i64 }

def main (xs: [][]f64) i j = (.[i:j]) <| iota (i + j)
