-- Indexing an array literal with a constant should remove the
-- indexing.
--
-- ==
-- structure { Index 0 Assert 0 }

fun [int] main([int] xs) =
  let xss = [xs]
  in xss[0]
