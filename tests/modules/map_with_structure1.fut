-- Testing whether it is possible to use a function
-- from a module in a curry function (map)
-- ==
-- input {
--   [1, 2, 3 ,4, 5, 6, 7, 8, 9, 10]
-- }
--  output {
--  55
-- }

module F = {
  def plus (a: i32) (b: i32) : i32 = a + b
}

def main (a: []i32) : i32 = reduce F.plus 0 a
