-- A simple locally defined function.  This one has free variables.
-- ==
-- input { 3 [1,2,3] } output { [4,5,6] }

def main [n] (x: i32) (a: [n]i32) =
  let add_x (y: i32) = x + y
  in map add_x a
