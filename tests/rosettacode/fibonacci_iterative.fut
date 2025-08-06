-- https://rosettacode.org/wiki/Fibonacci_sequence
-- ==
-- input { 0 } output { 0 }
-- input { 1 } output { 1 }
-- input { 2 } output { 1 }
-- input { 3 } output { 2 }
-- input { 40 } output { 102334155 }

def main (n: i32) : i32 =
  let (a, _) = loop (a, b) = (0, 1) for _i < n do (b, a + b)
  in a
