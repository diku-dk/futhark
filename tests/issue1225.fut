let main [h][w] (pic: [h][w][3]u8) idxArr : [][][3]u8 =
  let wm1 = w-1
  in map2 (\(row: [][3]u8) i ->
             if i == 0
             then row[1:] :> [wm1][3]u8
             else if i == w-1
             then row[:w-1] :> [wm1][3]u8
             else concat_to wm1 (row[0:i]) (row[i+1:]))
          pic idxArr
