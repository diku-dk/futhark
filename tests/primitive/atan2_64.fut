-- Does the atan2_64 function work?
-- ==
-- input { [0f64, 1f64, 1f64, 0f64, -1f64, 1f64, -1f64] [0f64, 0f64, 1f64, 1f64, 1f64, -1f64, -1f64] }
-- output { [0f64, 1.570796f64, 0.785398f64, 0.000000f64, -0.785398f64, 2.356194f64, -2.356194f64] }

def main = map2 f64.atan2
