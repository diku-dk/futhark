-- Simple case; simplify away the loops.
-- ==
-- input { 10 2 } output { 2 }
-- structure { Loop 0 }

def main (n: i32) (a: i32) =
  loop x = a for _i < n do
    loop _y = x for _j < n do
      a
