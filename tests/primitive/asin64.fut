-- Does the sin64 function work?
-- ==
-- input { 0.0 } output { 0.0 }
-- input { -0.84147096 } output { -1.0 }
-- input { -8.742278e-8 } output { -8.742278e-8 }
-- input { 8.742278e-8 } output { 8.742278e-8 }


def main(x: f64): f64 = f64.asin(x)
