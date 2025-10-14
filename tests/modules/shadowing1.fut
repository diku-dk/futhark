-- M1.foo() calls the most recent declaration of number, due to M0.number()
-- being brought into scope of M1, overshadowing the top level definition of
-- number()
-- ==
-- input {
-- }
-- output {
--  6.0 6 6
-- }

type best_type = f64
def best_number () : best_type = 6.0

module M0 = {
  type best_type = i32
  def best_number () : best_type = 6

  module M1 = {
    def best_number () : best_type = 6
  }
}

def main : (f64, i32, i32) = (best_number (), M0.best_number (), M0.M1.best_number ())
