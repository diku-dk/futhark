type cell = {a: i64, b: i64}

def step'' (f: () -> cell) : cell =
  (\() -> (\() -> f ()) ()) ()

def step' (cell: cell) : cell =
  if cell.a > 0
  then step'' (\() -> cell with b = 0)
  else step'' (\() -> cell)

def step (cell: cell) : cell =
  step' cell

entry main [h] [w] (grid: *[h][w]cell) : (*[h][w]cell, bool) =
  loop (grid, looping) = (grid, true)
  while looping do
    (unflatten (flatten (map (map step) grid)), false)
