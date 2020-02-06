-- A stream reduction with a map-out part.
-- ==
-- compiled input { 10000 } auto output

let main (n: i32) =
  let f k (chunk: [k]i32) =
    let x = if k == 0 then 0 else unsafe chunk[0]
    in map (+x+1) (iota k)
  let xs = map_stream f (iota n)
  in (xs, reduce_stream (+) (\n (xs': [n]i32) -> i32.sum xs') xs)
