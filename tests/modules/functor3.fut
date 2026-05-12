-- Parametric module where the argument contains an abstract type.
-- ==
-- input {} output {2}

module type colour = {
  type colour
}

module rgba_colour : colour = {
  type colour = i32
}

module colourspace (C: colour) = {
  open C

  def frob (x: colour) : colour = x
}

module rgba = colourspace (rgba_colour)

def main = 2
