-- Constant-forming a loop with a non-default type.
-- ==
-- input { 10i16 }
-- output { 100i16 }
-- structure { Loop 0 }

def main (x: i16) =
  loop acc = 0 for i < x do acc + x
