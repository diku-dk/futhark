-- Test that we can remove a single-iteration loop.
-- ==
-- structure { DoLoop 0 }

fun main(x: int, y: int): int =
  loop (x) = for i < 1 do
    x + y
  in x
