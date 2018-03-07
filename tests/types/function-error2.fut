-- Anonymous array element type misused.
-- ==
-- error: Argument of type .* passed for parameter of type .*

let reverse [n] [m] 't (a: [m][n]t): [m][n]t = a[::-1]

let main (x: []i32) = reverse x
