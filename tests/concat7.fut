-- Indexing into a concat across inner dimension.  The simplifier
-- should remove the concat.
--
-- ==
-- input { [[1,1],[2,2],[3,3]] [[4],[5],[6]] 1 2 } output { 5 }
-- structure { Concat 0 }

fun main(as: [][]int, bs: [][]int, i: int, j: int): int =
  let cs = concat@1 as bs
  in cs[i,j]
