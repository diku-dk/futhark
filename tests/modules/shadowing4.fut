-- Shadowing of types should work as expected.
-- ==
-- input { } output { 2 }

type t = i32

module m = {
  type t = t
}

def main : t = 2
