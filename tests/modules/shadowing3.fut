-- Shadowing of ordinary names should work as expected.
-- ==
-- input { 3 } output { 5 }

def plus2 x = x + 2

module m = {
  def plus2 = plus2
}

-- Should refer to the global one.

def main = m.plus2
