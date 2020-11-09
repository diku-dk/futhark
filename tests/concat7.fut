-- Indexing into a concat across inner dimension.  The simplifier
-- should remove the concat.
--
-- ==
-- input { [[1,1],[2,2],[3,3]] [[4],[5],[6]] 1 2 } output { 5 }
-- structure { Concat 0 }

let main [n][m] (as: [][n]i32) (bs: [][m]i32) (i: i32) (j: i32): i32 =
  let cs = map2 (concat_to (n+m)) as bs
  in cs[i,j]
