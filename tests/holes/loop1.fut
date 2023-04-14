-- Loop optimisation should not eliminate holes.
-- ==
-- input { 0i32 } error: hole

def holey (x: i32) : i32 = ???

def main (x: i32) = loop acc = holey x for i < x do acc
