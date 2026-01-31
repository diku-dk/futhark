-- Loop-variant sizes should not escape into types of values bound outside the loop.
-- ==
-- error: Loop-variant size

def grow_dim [h] (m: [h]f64) : []f64 =
  map (\i -> 0.0) (iota (h + 1))

def shrink_dim [h] (input2d: [h]f64) : []f64 =
  map (\i -> 0.0) (iota (h - 1))

def single_pass [d] (input2d: [d]f64) (factor: [d - 1 + 1]f64) : [d - 1 + 1]f64 =
  let shrunk = shrink_dim input2d
  let grown = grow_dim shrunk
  in map2 (*) grown factor

def multi_pass (input2d: []f64) (factor: []f64) : []f64 =
  loop input2d for i < 1 do
    single_pass input2d factor
