-- unsafe can only remove assertions on its sub-exp
-- ==
-- compiled input { 2 } output { 1 }
-- compiled input { 3 } error: Assertion is false

def main (x: i32) = assert (x % 2 == 0) (#[unsafe] x / 2)
