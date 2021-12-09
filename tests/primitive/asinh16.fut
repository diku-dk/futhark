-- Does the f16.asinh function work?
-- ==
-- input { [0f16, -0.84147096f16, -8.742278e-8f16, 8.742278e-8f16] }
-- output { [0f16, -0.7647251350294384f16, -8.742277999999989e-08f16, 8.742277999999989e-08f16] }

def main = map f16.asinh
