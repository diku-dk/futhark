-- ==
-- input { 2i64 3i64 }
-- output { [[[0i64, 0i64], [1i64, 1i64]], [[1i64, 1i64], [1i64, 1i64]], [[2i64, 2i64], [1i64, 1i64]]] }
-- input { 10i64 256i64 } auto output

entry main k n =
  #[incremental_flattening(only_intra)]
  tabulate n (\i -> let A = replicate k i
                    in tabulate k (\j -> if j % 2 == 1 then replicate k j else A))
