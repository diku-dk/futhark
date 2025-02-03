-- Somewhat exotic case related to entry points that are also used as
-- ordinary functions.

entry calculate_objective [d]
                          (xParam: [3][4*d]f64)
                          (yParam: [3][d]f64) : f64 =
  0

entry calculate_jacobian [d]
                          (mainParams: [3][4*d]f64)
                          (yParam: [3][d]f64) =
  vjp (\(x, y) -> calculate_objective x y) (mainParams, yParam) 1
