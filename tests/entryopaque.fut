-- It is OK for an entry point to return an opaque value, even when run with
-- 'futhark test', as long as we do not require any specific output.
-- ==
-- input { [1,2,3] [4,5,6] }

entry main (xs: []i32) (ys: []i32) = zip xs ys
