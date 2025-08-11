-- ==
-- input { empty([1][0]i32) }
-- output { [0i32] }

def main (foo: [1][0]i32) : [1]i32 =
  map2 (\_ _ -> 0) [0] foo
