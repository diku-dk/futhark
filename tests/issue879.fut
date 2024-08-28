-- ==
-- error: Consuming.*"s"

def f (xs: [10]i32) : [10]i32 = xs

def main (s: [10]i32) : *[10]i32 =
  let s = f s
  let s = loop s for _i < 10 do f s
  in s with [0] = 0
