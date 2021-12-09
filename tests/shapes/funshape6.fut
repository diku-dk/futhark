-- Based on issue 1351.
-- ==
-- error: Causality

def main (xs: [][]f64) i j = (.[i:j]) <| iota (i+j)
