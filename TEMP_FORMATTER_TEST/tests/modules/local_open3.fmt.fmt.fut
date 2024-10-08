-- Local open with nested modules, defined elsewhere.
-- ==
module has_x = {
  def x = 1i32
}

module has_has_x = {
  module has_x = has_x
}

def main = has_has_x(has_x.x)