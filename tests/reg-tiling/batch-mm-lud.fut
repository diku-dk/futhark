-- Batched matrix multiplication as it appears in Rodinia's LUD
-- ==
-- no_python compiled random input { [128][16][16]f32 [128][16][16]f32 [128][128][16][16]f32 } auto output

def main [m] [b]
         (Bs: [m][b][b]f32)
         (As: [m][b][b]f32)
         (Css: [m][m][b][b]f32) : *[m][m][b][b]f32 =
  let Btrs = map transpose Bs
  in map2 (\(Cs: [m][b][b]f32) (A: [b][b]f32) : [m][b][b]f32 ->
             map2 (\(C: [b][b]f32) (Btr: [b][b]f32) : [b][b]f32 ->
                     map2 (\(C_row: [b]f32) (A_row: [b]f32) : [b]f32 ->
                             map2 (\c B_col ->
                                     let prods = map2 (*) A_row B_col
                                     let sum = f32.sum prods
                                     in c - sum)
                                  C_row
                                  Btr)
                          C
                          A)
                  Cs
                  Btrs)
          Css
          As
