-- The power function has a dangerous kink for x==0.

-- ==
-- tags { autodiff }

-- ==
-- entry: fwd
-- input { 0.0f32 1.0f32 } output { 1.0f32 }
-- input { 0.0f32 0.0f32 } output { 0.0f32 }

-- ==
-- entry: rev
-- input { 0.0f32 1.0f32 } output { 1.0f32 0.0f32 }
-- input { 0.0f32 0.0f32 } output { 0.0f32 0.0f32 }

entry fwd x y : f32 = jvp (\(x, y) -> x ** y) (x, y) (1, 1)

entry rev x y = vjp (\(x, y) -> x ** y) (x, y) 1f32
