-- Will the abstract type defined in the argument to a parametric
-- module also be accessible in the resulting module?

module type has_cell = {
  type cell
}

module mk_has_cell (V: has_cell) = {
  type cell = V.cell
}

module i8_cell = {
                   type cell = i8
                 }:
                 has_cell

module m = mk_has_cell i8_cell

def main (x: m.cell) = x
