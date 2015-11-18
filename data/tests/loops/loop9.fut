-- Test that we can remove a single-iteration loop.
-- ==
-- structure { DoLoop 0 }

fun int main(int x, int y) =
  loop (x) = for i < 1 do
    x + y
  in x
