-- ==
-- tags { autodiff }
-- entry: fwd rev
-- input { 1f32 } output { -1f32 }
-- input { 3f32 } output { -1f32 }

def f x : f32 = -x

entry fwd x = jvp f x 1
entry rev x = jvp f x 1
