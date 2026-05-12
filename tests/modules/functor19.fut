-- Using a curried module twice should work, and notably not result in
-- anything being defined twice.
-- ==
-- input {} output {7}

module pm (A: {val x : i32}) (B: {val y : i32}) = {
  def z = A.x + B.y
}

module cm = pm {def x = 2}

module m1 = cm {def y = 1}
module m2 = cm {def y = 2}

def main = m1.z + m2.z
