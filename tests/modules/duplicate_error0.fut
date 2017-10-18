-- This test fails with a DuplicateDefinition error.
-- ==
-- error: .*Dup.*

module Foo = {
  let foo(): i32 = 1
}
let bar(): i32 = 1
let bar(): i32 = 2

let main(): i32 = 0
