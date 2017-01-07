-- Can we match a module with an unnamed signature?
-- ==
-- input { 5 } output { 7 }

module M: {val x: int} = {
  val x: int = 2
  val y: int = 3
}

fun main(x: int) = M.x + x
