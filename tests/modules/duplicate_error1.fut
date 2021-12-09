-- This test fails with a DuplicateDefinition error.
-- ==
-- error: .*Dup.*

module Foo = {
  let foo(): foo = 1
}
type foo = i32
type foo = float

def main(): i32 = 0
