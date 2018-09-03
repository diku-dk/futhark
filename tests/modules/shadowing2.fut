-- M0.foo() changes meaning inside M1, after the previous declaration of M0
-- is overshadowed.
--
-- ==
-- input {
-- }
-- output {
--  1 1 10
-- }

module M0 = {
  let foo(): i32 = 1
}

module M1 = {
  let bar(): i32 = M0.foo()
  module M0 = {
    let foo(): i32 = 10
  }
  let baz(): i32 = M0.foo()
}

let main: (i32, i32, i32) = (M0.foo(), M1.bar(), M1.baz())
