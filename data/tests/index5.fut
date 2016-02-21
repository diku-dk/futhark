-- Test that we can index a non-variable.
--
-- ==
-- input { 3 2 } output { 2 }

fun int main(int n, int i) =
  iota(n)[i]
