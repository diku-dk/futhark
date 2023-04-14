-- Loop optimisation should not eliminate holes, but also not
-- propagate them needlessly.
-- ==
-- input { 0i32 } output { 0i32 }
-- input { 1i32 } error: hole

def holey (x: i32) : i32 = ???

def main (x: i32) = loop acc = x for i < x do holey acc
