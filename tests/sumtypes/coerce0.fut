-- ==
-- input { true 3i64 }
-- output { [0i64,1i64,2i64] }
-- input { false 3i64 }
-- output { [0i64] }

type opt 't = #some t | #none

def f b (x: i64) =
  if b
  then #some (iota x)
  else (#none : opt ([1]i64)) :> opt ([]i64)

def main b x =
  let v = f b x
  in match v
     case #some arr -> arr
     case #none -> [0]
