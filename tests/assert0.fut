-- Basic assertion
-- ==
-- input { 2 } output { 1 }
-- input { 3 } error: x % 2 == 0

def main (x: i32) = assert (x % 2 == 0) (x / 2)
