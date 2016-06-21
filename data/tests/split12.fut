-- Split on an inner dimension.
-- ==
-- input { [[[1,2],[2,3]], [[3,4],[4,5]], [[5,6],[6,7]]] }
-- output { ([[[1,2], [3,4], [5,6]]], [[2,3], [4,5], [6,7]]) }

fun ([][][]int, [][][]int) main([][][]int xs) =
  split@1((1), xs)
