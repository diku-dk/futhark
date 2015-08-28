-- Distribute a redomap inside of a map.
--
-- Expected structure:
--
-- map
--   map
-- map
--   reduce
-- ==
--
-- structure distributed { Kernel 2 Reduce 0 Map 0 }

fun [int] main([[int]] a) =
  map(fn int ([int] a_r) =>
        reduce(+, 0, map(+1, a_r)),
      a)
