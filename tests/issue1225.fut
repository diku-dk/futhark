def main [h][w] (pic: [h][w][3]u8) idxArr : [][][3]u8 =
  let wm1 = w-1
  in map2 (\(row: [][3]u8) i ->
             if i == 0
             then resize wm1 row[1:]
             else if i == w-1
             then resize wm1 row[:w-1]
             else resize wm1 (concat (row[0:i]) (row[i+1:])))
          pic idxArr
