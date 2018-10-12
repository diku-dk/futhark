-- Local functions should not have their (overloaded) record type fixed
-- immediately.
-- ==
-- input { 2u8 } output { 3u8 }

let main (x: i32) (y: i32) =
  let f v = v.1
  in f (x,y)
