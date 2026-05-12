-- Make sure a type from an opened module inside a functor is not
-- abstract.
--
-- ==
-- input { }
-- output { 2 }

module f1 (R0: {type cell}) = {
  type cell = R0.cell
}

module f2 (R1: {type cell}) = {
  module L = f1 (R1)
  open L
  def id (x: cell) = x
}

module m2 = f2 ({type cell = i32})

def main : m2.cell = m2.id 2
