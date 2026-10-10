-- A loop result whose row-major leading stride is itself a compound
-- expression (here the product of two sizes) must still be recognised
-- as directly laid out when streamed to global memory.
-- ==
-- random input { [2][4][8][3]f32 } auto output

entry main [g][m][n][k] (a: [g][m][n][k]f32) =
  #[flattening(only_intra)]
  #[intrablock_result(global)]
  map (\mat -> loop acc = mat for _i < 3 do map (map (map (2*))) acc) a
