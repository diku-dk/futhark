-- | file: error.fut

type maybe 'a = #just a | #nothing

def two_two_three [n] (_: [n]u8) : maybe ([]i64) =
  if n == 0
  then #just []
  else #just [2, 2, 3]

entry f s =
  match two_two_three s
  case #just s' -> s'
  case #nothing -> []

-- ==
-- entry: f
-- input { "uuv" }
-- output { [2i64, 2i64, 3i64] }
