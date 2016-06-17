-- Using unsafe we can avoid a bounds check.
--
-- ==
-- structure { Assert 0 }

fun int main([]int a, int i) =
  unsafe a[i]
