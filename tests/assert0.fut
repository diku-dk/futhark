-- Basic assertion
-- ==
-- input { 3 } output { 0 }
-- input { 2 } error: x % 2 != 0
-- input { 0 } error: x % 2 != 0

def main (x: i32) = assert (x % 2 != 0) (2 / x)
