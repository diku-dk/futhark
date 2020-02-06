-- No hiding negative positions through type abbreviations!
-- ==
-- error: Size parameter "n" must be used

type^ ft [n] = i32 -> [n]i32

let f [n] (_: ft [n]) = 0
