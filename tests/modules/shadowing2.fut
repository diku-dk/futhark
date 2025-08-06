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
  def foo () : i32 = 1
}

module M1 = {
  def bar () : i32 = M0.foo ()

  module M0 = {
    def foo () : i32 = 10
  }

  def baz () : i32 = M0.foo ()
}

def main : (i32, i32, i32) = (M0.foo (), M1.bar (), M1.baz ())
