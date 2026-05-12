-- Defunctionaliser generated wrong uniqueness for this one at one
-- point.

def main [n] (xs: [n]i32) (ys: [n]i32) (is: []i32) =
  let op (xs': [n]i32, ys') i =
    ( if i == 2 then xs else xs'
    , ys'
    )
  in foldl op (xs, ys) is
