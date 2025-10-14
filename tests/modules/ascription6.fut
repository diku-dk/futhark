-- Multiple levels of ascription.
-- ==
-- input {} output { 4 2 }

module outer : {val x : i32 module inner: {val y : i32}} = {
  module inner : {val y : i32} = {
    def y = 2
  }

  def x = inner.y + 2
}

def main = (outer.x, outer.inner.y)
