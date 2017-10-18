-- The module opens a new environment, which lets us use names again, which were used
-- in a previous scope.
-- ==
-- input { }
-- output { 1 2.0 }

type foo = i32
module Foo = {
  let foo(): i32 = 1
  module Foo = {
    type foo = f64
    let foo(): foo = 2.0
  }
}

let main(): (foo, Foo.Foo.foo) = ( Foo.foo() , Foo.Foo.foo())