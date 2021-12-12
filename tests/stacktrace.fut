-- We can produce useful stack traces on errors, at least for
-- non-recursive functions.
-- ==
-- input { [1] 1 }
-- error: stacktrace.fut:7.*stacktrace.fut:9.*stacktrace.fut:11.*stacktrace.fut:13

def f (xs: []i32) (i: i32) = xs[i]

def g (xs: []i32) (i: i32) = f xs i

def h (xs: []i32) (i: i32) = g xs i

def main (xs: []i32) (i: i32) = h xs i
