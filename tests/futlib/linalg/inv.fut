-- ==
-- input { [[1.0f32, 2.0f32, 1.0f32], [2.0f32, 1.0f32, 1.0f32], [1.0f32, 1.0f32, 2.0f32]] }
-- output { [[-0.25f32, 0.75f32, -0.25f32], [0.75f32, -0.25f32, -0.25f32], [-0.25f32, -0.25f32, 0.75f32]] }

import "/futlib/math"
import "/futlib/linalg"

module f32linalg = linalg(f32)

let main(A: [#n][#n]f32): [n][n]f32 =
  f32linalg.inv A
