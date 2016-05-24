-- Expected distributed structure:
--
-- map
--   map
--     scan
-- map
--   map
--     scan
--
-- ==
--
-- structure distributed { Map 0 MapKernel 4 ScanKernel 4 }

fun [[[int]]] main([[[int,m],n]] a) =
  map(fn [[int,n],m] ([[int]] a_row) =>
        let b = map(fn [int] ([int] a_row_row) =>
                      scan(+, 0, a_row_row)
                   , a_row) in
        map(fn [int] ([int] b_col) =>
              scan(+, 0, b_col)
           , transpose(b))
     , a)
