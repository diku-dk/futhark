-- After flattening, the false branch aliases xsss unless we take care to avoid
-- it.

entry main (b: bool) (xsss: [][][]f32) =
  map (\xss ->
         if b
         then map (\xs ->
                     let s = f32.sum xs
                     in map (+ s) xs)
                  xss
         else reverse xss)
      xsss
