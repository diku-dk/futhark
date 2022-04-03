-- Does the sin64 function work?
-- ==
-- input { [0f64, -1f64, 3.1415927f64, -3.1415927f64] }
-- output { [0f64,  -0.84147096f64, -8.742278e-8f64, 8.742278e-8f64] }

def main = map f64.sin
