-- Does opening multiple modules shadow correctly?
-- ==
-- input { } output { 6 }

let the_value = 2

module M1 = {
  let the_value = 4
}

module M2 = {
  let the_value = 6
}

open M1 M2

let main() = the_value
