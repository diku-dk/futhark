-- ==
-- input { 2 } output { 2 }
module type has_cell =
{
  type cell
}

module type same_cell_twice =
{
  type cell
  include has_cellwith cell  = cell
}

module functor (V: {type cell}): same_cell_twice = {
  type cell = V.cell
}

module applied = functor {type cell = bool}

-- We can't create a value of this type, but let's just refer to it.
entry quux (x: applied.cell) = x

def main (x: i32) = x