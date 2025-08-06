-- CSE of things that don't look equal at first glance.
-- ==
-- input { 1 3 } output { 4 4 }
-- structure { BinOp 1 }

def main (x: i32) (y: i32) = (x + y, y + x)
