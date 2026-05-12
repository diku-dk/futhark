def main [n] (m: i32) (xs: [n]i32) : [n]i32 =
  let foo =
    loop xs = copy xs
    for _d < m do
      xs
  let foo[n - 1] = 0
  in foo
