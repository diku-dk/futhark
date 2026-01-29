-- Using a tuning parameter inside a parallel section.
-- ==
-- compiled no_wasm input { 1i64 [5i64] }
-- output { [8i64] }

entry main (x: i64) (ys: []i64) = map (\y -> (#[param(foo)] x) + y) ys
