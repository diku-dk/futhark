-- We can produce useful stack traces on errors, at least for
-- non-recursive functions.
-- ==
-- input { [1] 1 }
-- error: stacktrace.fut:7.*stacktrace.fut:9.*stacktrace.fut:11.*stacktrace.fut:13

let f (xs: []i32) (i: i32) = xs[i]

let g (xs: []i32) (i: i32) = f xs i

let h (xs: []i32) (i: i32) = g xs i

let main (xs: []i32) (i: i32) = h xs i
