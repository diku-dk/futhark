-- ==
-- compiled random input { [1024]bool [1024][256]i32 } auto output
-- structure distributed {
--   /If/True/SegMap/If/True/SegMap 1
--   /If/True/SegMap/If/False/SegMap 1
-- }

let main (bs: []bool) (xss: [][]i32) =
  map2 (\b xs -> if b then map (+2) xs else map (+3) xs) bs xss
