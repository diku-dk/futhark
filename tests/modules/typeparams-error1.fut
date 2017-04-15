-- Erroneous use of a parametric type inside a module type.
-- ==
-- error: i32matrix

module type MT = {
  type vector 'a
  type i32matrix = [](vector i32)
}

module M1: MT = {
  type vector 'a = []a
  type i32matrix = [][]f32
}

fun main() = 2
