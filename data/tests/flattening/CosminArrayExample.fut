// Problem here is that we need will distribute the map
// let arrs = map (\x -> iota(2*x)) xs
// let arr's = map (\x arr -> reshape( (x,2), arr) $ zip xs arrs
// let res = map(\arr' -> reduce(op+, 0, arr')) arr's
fun [int] main ([int] xs) =
  map(fn int (int x) =>
        let arr = iota(2 * x) in
        let arr' = reshape( (x,2), arr) in
            reduce(op+, 0, arr')
     , xs)
