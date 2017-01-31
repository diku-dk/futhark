-- Open and functors must work together.

module type mt = { val x: i32 }

module m1: mt = { val x = 2 }

module f(M: mt) = {
  open M
  val y = x + 2
}

module m2 = f(m1)

fun main() = m2.x + m2.y
