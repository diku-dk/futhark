def foo [n] [m] (img: [n][m]u32) : [n][m]u32 =
  map (map id) img

-- > :img foo (io.loadimg "../assets/ohyes.png")
