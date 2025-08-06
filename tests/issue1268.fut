type~ csc_mat =
  { col_offsets: []i64
  , row_idxs: []i32
  }

def low (d: csc_mat) (j: i64) : i64 = 0

entry foo (m: csc_mat) : csc_mat =
  let n = length m.col_offsets - 1
  let m' = copy m
  let lows' = map (\j -> low m' j) (iota n)
  in m'
