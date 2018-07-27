-- Shadowing of ordinary names should work as expected.
-- ==
-- input { 3 } output { 5 }

let plus2 x = x + 2

module m = {
  let plus2 = plus2 -- Should refer to the global one.
}

let main = m.plus2
