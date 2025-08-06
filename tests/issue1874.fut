type~ state = [][]f32

entry init (m: i64) : state =
  unflatten (replicate (m * m) 1f32)

def step' [m] (cells: *[m][m]f32) : *[m][m]f32 =
  let step_cell (cell: f32) ((y, x): (i64, i64)) =
    let x =
      if y > 0
      then cells[y - 1, x]
      else 0
    in cell * x
  in map2 (map2 step_cell) cells (tabulate_2d m m (\y x -> (y, x)))

entry step (cells: state) : state =
  step' (copy cells)

-- ==
-- entry: step
-- script input { init 10i64 }
