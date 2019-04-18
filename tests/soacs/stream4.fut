-- A stream reduction with a map-out part.
-- ==
-- compiled input { 10000 } auto output

let main (n: i32) =
  let f [k] (chunk: [k]i32) =
    if k == 0 then [] else unsafe map (+chunk[0]+1) (iota k)
  let xs = stream_map f (iota n)
  in (xs, stream_red (+) i32.sum xs)
