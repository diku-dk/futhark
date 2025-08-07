-- Uniqueness stuff.
-- ==

module M
  : {
      val f : *[1]f32 -> bool
    } = {
  def f (_: [1]f32) = true
}

def main (_: []f32) = M.f
