-- ==
-- tags { autodiff }
-- entry: fwd rev
-- input { true } output { true }

def f x : bool = !x

entry fwd x = jvp f x true
entry rev x = jvp f x true
