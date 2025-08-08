-- Global variables may not be unique!
-- ==
-- error: constant

def global : *[]i32 = [1, 2, 3]

def main (x: i32) =
  global with [0] = x
