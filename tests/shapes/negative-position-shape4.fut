-- No hiding negative positions through type abbreviations!
-- ==
-- error: Shape parameter `n` must first be given .*

type ft [n] = i32 -> [n]i32
