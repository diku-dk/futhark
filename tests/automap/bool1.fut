-- ==
-- entry: f
-- compiled input { [true, true, false] [false, true, true] }
-- output { [true, true, true] }

entry f (xs: []bool) (ys: []bool) = xs || ys
