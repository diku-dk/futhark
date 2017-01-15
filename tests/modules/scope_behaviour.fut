-- ==
-- input {
--   10 3.0
-- }
-- output {
-- 1 2 3
-- }
type foo = (int, f64)

module M0
  {
    type foo = (f64, int)
    type bar = foo
  }

module M1
  {
    type foo = f64
    type bar = M0.bar -- type is defined at line 13

    module M0
      {
        type foo = M0.foo -- is defined at line 12
        type bar = (int, int, int)
      }

    type baz = M0.bar -- defined at line 24
  }

type baz = M1.baz -- is defined at line 27

fun main(a: int, b: f64): baz = (1,2,3)

