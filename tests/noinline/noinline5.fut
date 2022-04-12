-- From issue #1634.
-- ==
-- input { [1i64,2i64] }
-- output { [1i64, 2i64, 1i64, 2i64] }

#[noinline]
def double [n] (A: [n]i64) : *[]i64 =
  A ++ A

def main [n] (A: [n]i64) : *[]i64 =
  double A
