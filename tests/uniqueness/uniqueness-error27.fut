-- You may not consume a free variable inside of a lambda.
--
-- ==
-- error: Consuming variable "a"

def consume (a: *[]i32) : []i32 = a

def main (a: *[]i32) : [][]i32 =
  map (\i -> consume a) (iota 10)
