-- No hiding negative positions through type abbreviations!
-- ==
-- error: Shape parameter "n" must first be used

type^ ft [n] = i32 -> [n]i32

let f [n] (_: ft [n]) = 0
