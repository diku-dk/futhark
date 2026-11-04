-- ==
-- entry: foo
-- compiled input { 2i64 } output { 3i64 }

-- This one should stay.
entry foo (x: i64) = #[param(foo)] x + 2

-- This one should at least not crash.
entry bar (x: i32) = #[param(bar)] x + 2
