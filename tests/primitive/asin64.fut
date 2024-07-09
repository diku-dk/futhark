-- Does the sin64 function work?
-- ==
-- input { [0.0, -0.84147096, -8.742278e-8, 8.742278e-8] }
-- output { [0.0, -1.0, -8.742278e-8, 8.742278e-8] }

def main = map f64.asin
