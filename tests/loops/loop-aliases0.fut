-- Regression test for alias inference in loops with mixed parameter diets.
-- ==
-- input { [0f32,1f32,2f32,3f32,4f32] [0f32,1f32,2f32,3f32,4f32,5f32,6f32,7f32,8f32,9f32] }
-- output { [0f32,1f32,2f32,3f32,4f32] [0f32,1f32,2f32,3f32,4f32,5f32,6f32,7f32,8f32,9f32] }

entry main (cache: *[5]f32) (ps: [10]f32) =
  loop (cache: *[5]f32, ps: [10]f32) = (cache, ps)
  for i < 5 do
    (cache, ps)
