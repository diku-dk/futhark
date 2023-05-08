module M: {val x: i32} = {
  def x: i32 = 2
  def y: i32 = 3
}

def main(x: i32) = M.x + x
