-- ==
-- error: "arr"

type^ arr = [2]i32

type bad = [3]arr

-- Bad, because we declared 'arr' to be lifted.
