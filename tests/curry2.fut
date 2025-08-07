-- Curry a simple function.
-- ==
-- input {
--   [8,5,4,3,2,1]
-- }
-- output {
--   [9,6,5,4,3,2]
-- }

def add (x: i32) (y: i32) : i32 = x + y

def main (a: []i32) : []i32 =
  map (add (1)) a
