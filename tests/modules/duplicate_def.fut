-- testing that variable shadowing and chunking
-- doesn't allow for duplicate definitions
-- ==
-- error: Duplicate
type foo = (i32, f32)

module M0 = {
  type foo = foo

  -- the type is defined from l. 1
  type bar = f32
}

module M1 = {
  type foo = f32
  type bar = M0.bar

  -- type is defined from l.5

  module M0 = {
    type foo = M0.foo

    -- is defined at l. 5
    type bar = (i32, i32, i32)
  }

  type foo = f32

  -- REDEFINITION OF foo IN Struct M1
  type baz = M0.bar
}

-- defined at line 17

type baz = M1.baz

-- is defined at l. 13

def main (a: i32, b: float) : baz = (1, 2, 3)
