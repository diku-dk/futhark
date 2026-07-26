-- Nonuniformly replicating a regular array.
-- ==
-- input { [1i64,2i64] [0, 1] [[4,5],[5,6]] }
-- output { [[4,5],[5,6]] }
-- structure gpu { SegScan 4 /Opaque 1 }

def main =
  map3 (\n (i: i32) (x: [2]i32) ->
          let A = opaque (replicate n x)
          in #[unsafe] A[i])
