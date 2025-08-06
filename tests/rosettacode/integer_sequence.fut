-- https://rosettacode.org/wiki/Integer_sequence
--
-- Infinite loops cannot produce results in Futhark, so this program
-- accepts an input indicating how many integers to generate.
--
-- ==
-- input { 10i64 } output { [0i64,1i64,2i64,3i64,4i64,5i64,6i64,7i64,8i64,9i64] }

def main (n: i64) : [n]i64 = iota n
