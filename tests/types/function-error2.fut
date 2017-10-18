-- Anonymous array element type misused.
-- ==
-- error: reverse

let reverse [n] [m] 't (a: [m][n]t): [m][n]t = a[::-1]

let main (x: []i32) = reverse x
