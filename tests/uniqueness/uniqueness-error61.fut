-- Based on #1842
--
-- The problem was that type ascription of a function type did not
-- check that the uniqueness matched.
-- ==
-- error: does not have expected type

def f (xs: *[]f32) : f32 = 0f32
entry g : []f32 -> f32 = f
