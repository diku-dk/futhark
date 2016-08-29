-- This program tests that a transposition is carried out correctly
-- even when the destination has an offset.  The OpenCL code generator
-- once messed this up.
--
-- ==
-- input { [[1,2,3]] [[4,7],[5,8],[6,9]] [[10,11,12]] }
-- output { [[1,2,3], [4,5,6],[7,8,9], [10,11,12]] }

fun main(a: [][n]int, b: [n][]int, c: [][n]int): [][]int =
  concat a (transpose(b)) c
