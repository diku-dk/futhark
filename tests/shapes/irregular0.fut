-- Irregularity must be detected!
-- ==
-- input {} error:

let main = ([([1], [2,3]), ([2,3], [1]) :> ([1]i32, [2]i32)])[1]
