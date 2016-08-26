-- Indexing an array literal with a constant should remove the
-- indexing.
--
-- ==
-- structure { Index 0 Assert 0 }

fun main(xs: []int): []int =
  let xss = [xs]
  in xss[0]
