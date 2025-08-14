-- Projecting an array index should be permitted.
-- ==
-- input { 0 }
-- output { 0 }

def main (x: i32) = let a = [(x, x)] in a[0].0
