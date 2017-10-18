-- https://rosettacode.org/wiki/Integer_sequence
--
-- Infinite loops cannot produce results in Futhark, so this program
-- accepts an input indicating how many integers to generate.
--
-- ==
-- input { 10 } output { [0,1,2,3,4,5,6,7,8,9] }

let main(n: i32): [n]i32 = iota n
