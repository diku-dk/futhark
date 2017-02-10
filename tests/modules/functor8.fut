-- Using the same signature in multiple places should not cause
-- trouble.

module type rules = {
  type cell
}

module f1(R1: rules) = {
  type cell = R1.cell
}

module f2(R2: rules) = {
  module L = f1(R2)
}

module conway = f2({type cell = bool})
