import "/futlib/math"
import "/futlib/vector"

module vec3 = mk_vec3 f64
let unvec3 ({x,y,z}: vec3.vec) = (x,y,z)

-- ==
-- entry: test_add
-- input { 0.0 1.0 2.0 3.0 4.0 5.0 }
-- output { 3.0 5.0 7.0 }

entry test_add (x1: f64) (y1: f64) (z1: f64) (x2: f64) (y2: f64) (z2: f64) =
  unvec3 ({x=x1,y=y1,z=z1} vec3.+ {x=x2,y=y2,z=z2})

-- ==
-- entry: test_sub
-- input { 3.0 5.0 7.0 3.0 4.0 5.0 }
-- output { 0.0 1.0 2.0 }

entry test_sub (x1: f64) (y1: f64) (z1: f64) (x2: f64) (y2: f64) (z2: f64) =
  unvec3 ({x=x1,y=y1,z=z1} vec3.- {x=x2,y=y2,z=z2})

-- ==
-- entry: test_dot
-- input { 0.0 1.0 2.0 3.0 4.0 5.0 }
-- output { 14.0 }

entry test_dot (x1: f64) (y1: f64) (z1: f64) (x2: f64) (y2: f64) (z2: f64) =
  vec3.dot {x=x1,y=y1,z=z1} {x=x2,y=y2,z=z2}

-- ==
-- entry: test_scale
-- input { 0.0 3.0 4.0 5.0 }
-- output { 0.0 0.0 0.0 }
-- input { 2.0 3.0 4.0 5.0 }
-- output { 6.0 8.0 10.0 }

entry test_scale (s: f64) (x1: f64) (y1: f64) (z1: f64) =
  unvec3 (vec3.scale s {x=x1,y=y1,z=z1})

-- ==
-- entry: test_norm
-- input { 0.0 4.0 3.0 }
-- output { 5.0 }
-- input { 3.0 4.0 0.0 }
-- output { 5.0 }
-- input { 0.0 0.0 0.0 }
-- output { 0.0 }

entry test_norm (x1: f64) (y1: f64) (z1: f64) =
  vec3.norm {x=x1,y=y1,z=z1}

-- This one would be nicer with property-based testing.
-- ==
-- entry: test_normalise
-- input { 10.0 10.0 10.0 }
-- output { 0.577350 0.577350 0.577350 }
-- input { 2.0 4.0 6.0 }
-- output { 0.267261 0.534522 0.801783 }

entry test_normalise (x1: f64) (y1: f64) (z1: f64) =
  unvec3 (vec3.normalise {x=x1,y=y1,z=z1})
