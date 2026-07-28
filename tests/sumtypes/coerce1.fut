-- ==

type opt 't = #some t | #none

def f b (x: i64) =
  if b
  then #some (iota x)
  else #none :> opt ([]i64)
