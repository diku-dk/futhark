-- A simple case of a parametric type.
-- ==
-- input { 2i64 } output { [0i64,1i64] }

type~ vector 't = []t

def main (n: i64) : vector i64 = iota n
