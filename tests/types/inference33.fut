-- Local functions should not have their (overloaded) type fixed
-- immediately, but rather wait until the top-level function has to be
-- assigned a type.
-- ==
-- input { 2u8 } output { 3u8 }

let main (x: u8) =
  let inc y = y + 1
  in inc x
