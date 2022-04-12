-- From issue #1634.

#[noinline]
def double [n] (A: [n]i64) : *[]i64 =
  A ++ A

def main [n] (A: [n]i64) : *[]i64 =
  double A
