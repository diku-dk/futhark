-- ==
-- input {
--   [[-1f32, 0f32], [0.98f32, -0.2f32], [-0.32f32, -0.95f32], [-0.71, -0.71]]
--   [-1.77f32, 1.72f32, -2.41f32, -2.81f32]
-- }
-- output {
--   [1.9734432f32, 1.8890195f32]
-- }

import "futlib/math"
import "futlib/linalg"

module f32linalg = linalg(f32)

fun main (X: [n][m]f32) (b: [n]f32): [m]f32 = f32linalg.ols X b
