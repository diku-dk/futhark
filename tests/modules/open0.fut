-- Does the open declaration work at all?
-- ==
-- input { } output { 4 }

module M = {
  let the_value = 4
}

open M

fun main() = the_value