-- Somewhat exotic case related to entry points that are also used as
-- ordinary functions.


-- ==
-- tags { autodiff }

entry calculate_objective [d]
                          (xParam: [3][4 * d]f32)
                          (yParam: [3][d]f32) : f32 =
  0

entry calculate_jacobian [d]
                         (mainParams: [3][4 * d]f32)
                         (yParam: [3][d]f32) =
  vjp (\(x, y) -> calculate_objective x y) (mainParams, yParam) 1
