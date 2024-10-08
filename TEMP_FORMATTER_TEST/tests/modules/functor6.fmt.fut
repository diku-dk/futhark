-- Applying a parametric module inside another parametric module.
-- ==
-- input { }
-- output { 2 }
module f1 (R: {type cell}) = {
  type cell = R.cell
}

module f2 (R: {type cell}) = {
  module L = f1 {R}
  
  type cell = R.cell
}

module m1 = {
  type cell = i32
}

module m2 = f2 {m1}

def main: m2.cell = 2