-- Once failed in kernel extraction.  The problem was that the map and
-- reduce are fused together into a redomap with a map-out array.
-- This was not handled correctly when it was turned into a
-- group-level stream.
--
-- ==
-- structure gpu { SegMap 2 SegMap/Loop 1 }

def indexOfMax8 ((x,i): (u8,i32)) ((y,j): (u8,i32)): (u8,i32) =
  if x < y then (y,j) else (x,i)

def max8 (max_v: u8) (v: u8): u8 =
  if max_v < v then v else max_v

def main [h][w] (frame : [h][w]i32) : [h][w]u8 =
  map (\row: [w]u8 ->
         let rs = map u8.i32 row
         let m = #[sequential] reduce max8 0u8 rs
         let rs' = map (max8 m) rs
         in rs')
   frame
