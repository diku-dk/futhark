-- Can we match a module with an unnamed signature?
-- ==
-- input { 5 } output { 7 }

module M : {val x : i32} = {
  def x : i32 = 2
  def y : i32 = 3
}

def main (x: i32) = M.x + x
