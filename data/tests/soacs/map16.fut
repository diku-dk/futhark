-- Map returning an array predicated on the index variable.
--
-- ==
-- input { 2 }
-- output { [[0], [1]] }

fun main(chunk: int): [][]int =
  map (fn (k: int): [1]int  =>
         if k==0 then [0] else [1]
     ) (iota(chunk))
