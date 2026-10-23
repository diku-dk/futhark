-- ==
-- input {
--   10 3.0f32
-- }
-- output {
-- 1 2 3
-- }
type foo = (i32, f32)

module M0 = {
  type foo = (f32, i32)
  type bar = foo
}

module M1 = {
  type foo = f32
  type bar = M0.bar

  -- type is defined at line 13

  module M0 = {
    type foo = M0.foo

    -- is defined at line 12
    type bar = (i32, i32, i32)
  }

  type baz = M0.bar
}

-- defined at line 24

type baz = M1.baz

-- is defined at line 27

def main (a: i32) (b: f32) = (1, 2, 3) : baz
