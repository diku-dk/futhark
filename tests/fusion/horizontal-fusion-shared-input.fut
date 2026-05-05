-- Horizontal fusion of two reductions that share the same input
-- array. After fusion, duplicate inputs should be eliminated.
-- ==
-- structure { Screma 3 }

def dotprod [n] (xs: [n]i32) (ys: [n]i32) : i32 =
  i32.sum (map2 (i32.*) xs ys)

entry main W A WH =
  let W_TA = map (\Wr -> map (\Ac -> dotprod Wr Ac) (transpose A)) W
  let W_TWH = map (\Wr -> map (\WHc -> dotprod Wr WHc) (transpose WH)) W
  let H_update = map2 (map2 (i32./)) W_TA W_TWH
  in H_update
