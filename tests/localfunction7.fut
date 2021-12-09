-- A local function can refer to a global variable.
-- ==
-- input { 1 } output { 5 }

def two = 2

def main (x: i32) =
  let add_two (y: i32) = y + two
  in add_two (add_two x)
