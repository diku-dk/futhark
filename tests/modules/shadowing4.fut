-- Shadowing of types should work as expected.
-- ==
-- input { } output { 2 }

type t = i32

module m = {
  type t = t
}

let main: t = 2
