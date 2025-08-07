-- ==
-- error: Unshared constructors

type t = #foo | #bar

def f b : t = if b then #foo else #baar
