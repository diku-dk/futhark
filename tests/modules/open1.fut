-- Does open shadow correctly?
-- ==
-- input { } output { 4 }

let the_value = 2

module M = {
  let the_value = 4
}

open M

fun main() = the_value
