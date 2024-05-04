let sgmSumInt [n] (flg : [n]i64) (arr : [n]i64) : {[n]i64 | \res-> is_indexfn res} =
  let zipped = zip flg arr
  let flgs_vals = 
    scan ( \ (f1, x1) (f2,x2) -> 
            if f2 > 0 then (f1 | f2, x2)
            else (f1 | f2, x1 + x2) )
         (0,0) (zip flg arr)
  let (_, vals) = unzip flgs_vals
  in vals
  -- let flgs_vals = 
  --   scan ( \ (f1, x1) (f2,x2) -> 
  --           let f = f1 | f2 in
  --           if f2 > 0 then (f, x2)
  --           else (f, x1 + x2) )
  --        (0,0) xs
  -- let (_, vals) = unzip flgs_vals
  -- in vals
