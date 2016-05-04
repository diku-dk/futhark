-- Only one of the accesses should be unsafe.
--
-- ==
-- structure { Assert 1 }

fun (int,int) main([int] a, int i, int j) =
  (unsafe a[i], a[j])
