-- Does the atan2_64 function work?
-- ==
-- input { 0f64 0f64 } output { 0f64 }
-- input { 1f64 0f64 } output { 1.570796f64 }
-- input { 1f64 1f64 } output { 0.785398f64  }
-- input { 0f64 1f64 } output { 0.000000f64 }
-- input { -1f64 1f64 } output { -0.785398f64 }
-- input { 1f64 -1f64 } output { 2.356194f64 }
-- input { -1f64 -1f64 } output { -2.356194f64 }

fun f64 main(f64 x, f64 y) = atan2_64(x,y)
