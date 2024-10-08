-- Open and functors must work together.
-- ==
-- input {} output {6}
module type mt = {val x: i32}

module m1: mt = {def x = 2}

module f (M: mt) = {
  open M
  
  def y = x + 2
}

module m2 = f {m1}

def main = m2.x + m2.y