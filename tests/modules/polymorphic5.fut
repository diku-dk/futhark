-- Removing polymorphism with an ascription.
-- ==
-- input { 2 } output { 2 }

module m: { val id : i32 -> i32 } = {
  let id x = x
}

let main (x: i32) = m.id x
