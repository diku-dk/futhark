-- A flatmap whose lambda itself contains a flatmap, exercising the nested
-- flattening path through an enclosing flatmap. Each element is expanded into
-- 'k' copies of itself, but via an inner flatmap.
-- ==
-- input { [0i64, 2i64, 3i64, 1i64] [5i32, 10i32, 20i32, 30i32] }
-- output { [0i64, 2i64, 3i64, 1i64]
--          [true, false, true, false, false, true]
--          [0i64, 0i64, 2i64, 5i64]
--          [10i32, 10i32, 20i32, 20i32, 20i32, 30i32] }
-- input { [2i64, 3i64, 1i64] [10i32, 20i32, 30i32] }
-- output { [2i64, 3i64, 1i64]
--          [true, false, true, false, false, true]
--          [0i64, 2i64, 5i64]
--          [10i32, 10i32, 20i32, 20i32, 20i32, 30i32] }

def main (ks: []i64) (xs: []i32) =
  flatmap (\(k, x) ->
             let (_, _, _, r) = flatmap (\(kk, y) -> replicate kk y) [(k, x)]
             in sized k r)
          (zip ks xs)
