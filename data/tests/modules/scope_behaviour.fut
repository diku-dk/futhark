-- ==
-- input {
--   10 3.0
-- }
-- output {
-- 1 2 3
-- }
type foo = (int, f32)

struct M0
  {
    type foo = foo -- the type is defined from l. 1
    type bar = f32
  }

struct M1
  {
    type foo = f32
    type bar = M0.bar -- type is defined from l.6

    struct M0
      {
        type foo = M0.foo -- is defined at l. 5
        type bar = (int, int, int)
      }

    type baz = M0.bar -- defined at line 17
  }

type baz = M1.baz -- is defined at l. 13

fun main(a: int, b: float): baz = (1,2,3)

