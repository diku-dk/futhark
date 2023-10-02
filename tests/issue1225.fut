def main [h][w] (pic: [h][w][3]u8) idxArr : [][][3]u8 =
  map2 (\(row: [][3]u8) i ->
          if i == 0
          then sized (w-1) row[1:]
          else if i == w-1
          then sized (w-1) row[:w-1]
          else sized (w-1) (concat (row[0:i]) (row[i+1:])))
       pic idxArr
