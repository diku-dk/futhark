import "/futlib/math"
import "/futlib/vec3"

open (mk_vec3 f64)

-- ==
-- entry: test_add
-- input { 0.0 1.0 2.0 3.0 4.0 5.0 }
-- output { 3.0 5.0 7.0 }

entry test_add (x1: f64) (y1: f64) (z1: f64) (x2: f64) (y2: f64) (z2: f64) =
  (x1,y1,z1) + (x2,y2,z2)

-- ==
-- entry: test_sub
-- input { 3.0 5.0 7.0 3.0 4.0 5.0 }
-- output { 0.0 1.0 2.0 }

entry test_sub (x1: f64) (y1: f64) (z1: f64) (x2: f64) (y2: f64) (z2: f64) =
  (x1,y1,z1) - (x2,y2,z2)

-- ==
-- entry: test_dot
-- input { 0.0 1.0 2.0 3.0 4.0 5.0 }
-- output { 14.0 }

entry test_dot (x1: f64) (y1: f64) (z1: f64) (x2: f64) (y2: f64) (z2: f64) =
  dot (x1,y1,z1) (x2,y2,z2)

-- ==
-- entry: test_scale
-- input { 0.0 3.0 4.0 5.0 }
-- output { 0.0 0.0 0.0 }
-- input { 2.0 3.0 4.0 5.0 }
-- output { 6.0 8.0 10.0 }

entry test_scale (s: f64) (x1: f64) (y1: f64) (z1: f64) =
  scale s (x1,y1,z1)

-- ==
-- entry: test_norm
-- input { 0.0 4.0 3.0 }
-- output { 5.0 }
-- input { 3.0 4.0 0.0 }
-- output { 5.0 }
-- input { 0.0 0.0 0.0 }
-- output { 0.0 }

entry test_norm (x1: f64) (y1: f64) (z1: f64) =
  norm (x1,y1,z1)

-- This one would be nicer with property-based testing.
-- ==
-- entry: test_normalise
-- input { 10.0 10.0 10.0 }
-- output { 0.577350 0.577350 0.577350 }
-- input { 2.0 4.0 6.0 }
-- output { 0.267261 0.534522 0.801783 }

entry test_normalise (x1: f64) (y1: f64) (z1: f64) =
  normalise (x1,y1,z1)
