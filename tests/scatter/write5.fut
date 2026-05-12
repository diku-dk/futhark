def main [n] ((a: [n]f32, ja: []i32)) : ([]f32, []i32) =
  let res = zip a ja
  let idxs = iota n
  in unzip (scatter (copy res) idxs res)
