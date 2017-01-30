-- Does open shadow correctly?
-- ==
-- input { } output { 4 }

val the_value = 2

module M = {
  val the_value = 4
}

open M

fun main() = the_value
