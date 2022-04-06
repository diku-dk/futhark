-- A stream reduction with a map-out part.
-- ==
-- compiled input { 100004i64 } auto output

def main (n: i64) =
  let f k (chunk: [k]i32) =
    let x = if k == 0 then 0 else chunk[0]
    in map (+x+1) (map i32.i64 (iota k))
  let xs = map_stream f (map i32.i64 (iota n))
  in (xs, reduce_stream (+) (\n (xs': [n]i32) -> i32.sum xs') xs)
