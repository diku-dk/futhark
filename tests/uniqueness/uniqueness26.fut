-- Carefully handle the case where there is a lot of alias overlapping
-- going on between the target of an in-place update and the source,
-- including inside a section!  It was actually the section that we
-- mishandled in lambda-lifting.
-- ==
-- input { 0i64 3i64 [3f32,4f32,5f32,6f32] }
-- output { [1.0f32, 1.3333334f32, 1.6666666f32, 6.0f32] }

def main [n] (i: i64) (j: i64) (A: *[n]f32) : []f32 =
  A with [i:j] = map (/ A[0]) A[i:j]
