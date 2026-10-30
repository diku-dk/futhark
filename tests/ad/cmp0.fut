-- The derivatives of comparisons are always zero, but we should at
-- least not crash.

-- ==
-- tags { autodiff }

-- ==
-- entry: fwd
-- input { 1.0f32 2.0f32 }
-- output { false false }
-- input { 1.0f32 1.0f32 }
-- output { false false }

entry fwd x y =
  ( jvp (\(a, b) -> f32.(a == b)) (x, y) (1, 0)
  , jvp (\(a, b) -> f32.(a == b)) (x, y) (0, 1)
  )

-- ==
-- entry: rev
-- input { 1.0f32 2.0f32 }
-- output { 0.0f32 0.0f32 }
-- input { 1.0f32 1.0f32 }
-- output { 0.0f32 0.0f32 }

entry rev x y =
  vjp (\(a, b) -> f32.(a == b)) (x, y) true
