-- Using a curried module twice should work, and notably not result in
-- anything being defined twice.  Also opening a submodule of the
-- second parameter.
-- ==
-- input {} output {7 7}

module pm (A: {val x : i32}) (B: {val y : i32}) = {
  def z = A.x + B.y
}

module cm = pm {def x = 2}

module m1 = cm {def y = 1}
module m2 = cm {def y = 2}

module pm_open (A: {val x : i32}) (B: {module n: {val y : i32}}) = {
  open B.n
  def z = A.x + y
}

module cm_open = pm_open {def x = 2}

module m3 = cm_open {module n = {def y = 1}}
module m4 = cm_open {module n = {def y = 2}}

def main = (m1.z + m2.z, m3.z + m4.z)
