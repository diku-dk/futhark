-- We cannot just ignore constraints imposed by a higher-order function.
-- ==
-- error: Sizes.*"n".*do not match

def f (g: (n: i64) -> [n]i32) (l: i64) : i32 =
  (g l)[0]

def main = f (\n : []i64 -> iota (n + 1))
