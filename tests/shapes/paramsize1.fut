-- ==
-- error: "k"

type^ f = (k: i64) -> [k]i32 -> i64

def f : f = \_ xs -> length xs

def main [K] (input: [K]i32) =
  f K input
