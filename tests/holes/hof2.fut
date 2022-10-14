-- Based on #1738.
-- ==
-- input { 0f32 } error: hole.*hof2.fut:7

def f a b : f32 = a + b

entry main y : f32 = (let x = y in f x) ???
