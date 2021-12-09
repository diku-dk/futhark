-- This test fails with a DuplicateDefinition error.
-- ==
-- error: .*Dup.*

module Foo = {
  let foo(): i32 = 1
}
def bar(): i32 = 1
def bar(): i32 = 2

def main(): i32 = 0
