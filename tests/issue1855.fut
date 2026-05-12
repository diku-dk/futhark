-- Pattern matching on an incompletely known sum type should still
-- respect aliases.

type t [n] = #foo [n](i32, i32) | #bar

def main (x: t []) =
  (\y ->
     match y
     case #foo arr -> take 2 arr
     case _ -> [(0, 1), (1, 2)]) x
