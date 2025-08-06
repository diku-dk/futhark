-- Local open with a nested module.
-- ==
-- input { 1 } output { 6 }

module m0 = {
  module m1 = {
    def x = 2
  }

  def x = 3
}

def main (x: i32) = x + m0.(x + m1.(x))
