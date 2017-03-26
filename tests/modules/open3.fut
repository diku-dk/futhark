-- Check that lookup of multiple modules in 'open' is simultaneous,
-- not parallel.
-- ==
-- input { }
-- output { 1 3 }

module M1 = {
  let x = 1
  module M2 = {
    let y = 2
  }
}

module M2 = {
  let y = 3
}

open M1 M2

let main() = (x, y)