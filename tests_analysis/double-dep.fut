def main [n] (xs: [n]i32) : [n]i32 =
  loop ys = copy xs for i < n do
    map (\p ->
      let t = i * 2 + p / 2 in
      ys[t] + ys[t / 2]
    ) (iota n)
