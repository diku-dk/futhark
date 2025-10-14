-- ==
-- input { 1i64 }
-- output { [true, true] [false, false] }

type^ t = ?[n].([n]bool, bool -> [n]bool)

def v x : t =
  let x' = x + 1
  in ( replicate x' true
     , \b -> replicate x' b
     )

def main x =
  let (arr, f) = v x
  in unzip (zip arr (f false))
