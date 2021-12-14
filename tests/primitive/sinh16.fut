-- Does the f16.sinh function work?
-- ==
-- input  { [0f16, -1f16, 3.1415927f16, -3.1415927f16] }
-- output { [0f16, -1.1752011936438014f16, 11.548739357257748f16, -11.548739357257748f16] }

def main = map f16.sinh
