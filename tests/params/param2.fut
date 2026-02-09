-- Using a tuning parameter inside a parallel section, where the default value
-- is variant.
-- ==
-- tags { no_ispc }
-- compiled no_wasm input { [5i64] }
-- output { [3i64] }

entry main (xs: []i64) = map (\x -> #[param(foo)] x) xs
