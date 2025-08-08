-- The merge parameter in a loop cannot have function type.
-- ==
-- error: used as loop variable

def id 'a (x: a) : a = x

def main (n: i32) =
  loop f = id
  for i < n do
    \(y: i32) -> f y
