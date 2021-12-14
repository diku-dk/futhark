-- Multiple slices with the same operands produce things that have the
-- same size.

def f (x: i64) = x + 2
def g (x: i64) = x * 2

def main [n] (xs: [n]i32) (ys: [n]i32) (i: i64) (j: i64) =
  zip xs[(f i):(g j)] ys[(f i):(g j)]
