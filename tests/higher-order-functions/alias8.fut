-- The instantiated type of "|>" has a fresh result because "g" constructs
-- its result freshly.  That must hold for a tuple result too, or the
-- monomorphic instance of "|>" claims less than the type checker concluded.
-- ==
-- input { [1,2,3] } output { [1,2,3] 0 }

def g (r: []i32) : (*[]i32, i32) = (copy r, 0)

entry main (r: []i32) = r |> g
