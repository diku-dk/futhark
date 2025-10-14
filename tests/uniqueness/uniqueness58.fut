def main to from (counts: *[]i32) (state: *[][]u8) =
  let state[to, counts[to]] = state[from, counts[from] - 1]
  let counts[to] = counts[to] + 1
  in (counts, state)
