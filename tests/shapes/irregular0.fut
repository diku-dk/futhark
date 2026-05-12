-- Irregularity must be detected!
-- ==
-- input {0} error:

def main (x: i32) = ([([1], [2, 3]), ([2, 3], [1]) :> ([1]i32, [2]i32)])[1]
