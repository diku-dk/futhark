def main [n] [m] (xsss: *[2][n][m]i32) =
  #[unsafe]
  let xss = xsss[0]
  let ys =
    loop acc = replicate m 0
    for i < m do
      let acc[i] = xsss[0, 0, i] + 1
      in acc
  in xss with [0] = ys
