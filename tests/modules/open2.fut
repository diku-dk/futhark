-- Does opening multiple modules shadow correctly?
-- ==
-- input { } output { 6 }

val the_value = 2

module M1 = {
  val the_value = 4
}

module M2 = {
  val the_value = 6
}

open M1 M2

fun main() = the_value
