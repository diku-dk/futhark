-- M1.foo() calls the most recent declaration of number, due to M0.number()
-- being brought into scope of M1, overshadowing the top level definition of
-- number()
-- ==
-- input {
-- }
-- output {
--  2
-- }

def number () : i32 = 1

module M0 = {
  def number () : i32 = 2

  module M1 = {
    def foo () : i32 = number ()
  }
}

def main : i32 = M0.M1.foo ()
