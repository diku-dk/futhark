-- Does the f16.tanh function work?
-- ==
-- input  { [0f16, 0.78539819f16, -0.78539819f16] }
-- output { [0f16, 0.6557942177943699f16, -0.6557942177943699f16] }

def main = map f16.tanh
