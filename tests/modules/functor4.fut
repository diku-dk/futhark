-- Another parametric module where the argument contains an abstract type.
-- ==
-- input {} output {2}

module type foo = {
  type foo

  val mkfoo: i32 -> foo
}

module rgba_foo: foo = {
  type foo = i32

  fun mkfoo (x: i32) = x
}

module foospace(C: foo) = {
  open C

  fun frob (x: foo): foo = x
}

module rgba = foospace(rgba_foo)

fun main() = 2
