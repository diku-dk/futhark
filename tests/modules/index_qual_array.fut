-- ==
-- input { 4 } output { 5 }

module M = {
  def a : []i32 = [1, 2, 3]
}

def main (x: i32) : i32 = M.a[0] + x
