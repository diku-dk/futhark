-- Test that parameter names are not lost in sections.

let flipreplicate x n = replicate n x

let main n (x: i32) = (x `flipreplicate`) n
