-- Based on #2593.
--
-- This test checks z-encoding in function applications.

#[noinline]
def wizard (x: f32) : f32 = x * x + 1

#[noinline]
def outerfn (x: f32) : f32 = wizard x + wizard (x + 1)

entry main (xs: []f32) : []f32 = map outerfn xs
