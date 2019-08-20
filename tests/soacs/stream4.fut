-- A stream reduction with a map-out part.
-- ==
-- compiled input { 10000 } auto output

let main (n: i32) =
  let f k (chunk: [k]i32) =
    if k == 0 then [] else unsafe map (+chunk[0]+1) (iota k)
  let xs = map_stream f (iota n)
  in (xs, reduce_stream (+) (const i32.sum) xs)
