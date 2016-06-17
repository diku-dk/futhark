-- Single-iteration maps should be simplified away.
--
-- ==
-- input { 2 } output { [4] }
-- structure { Map 0 }

fun [1]int main(int x) =
  map(+x, replicate(1, x))
