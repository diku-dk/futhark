-- Anonymous array element type misused.
-- ==
-- error: Cannot apply "reverse" to "x"

let reverse [n] [m] 't (a: [m][n]t) = a[::-1]

let main (x: []i32) = reverse x
