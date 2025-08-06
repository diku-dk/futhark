-- https://rosettacode.org/wiki/Even_or_odd
--
-- true if even.
-- ==
-- input { 0 } output { true }
-- input { 1 } output { false }
-- input { 10 } output { true }
-- input { 11 } output { false }

def main (x: i32) : bool = (x & 1) == 0
