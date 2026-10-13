-- We should not propagate the scratch to the size.
-- ==
-- input { 2i64 3i64 }
-- output { 18i64 }

entry main (n: i64) (m: i64) =
  let b = #[scratch] replicate (n * (m * m)) 0
  in length b
