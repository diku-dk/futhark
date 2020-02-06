-- Just because a loop has an unpredictable size, that does not mean
-- it has *any* size!  And in particular, not the size of the initial value.
-- ==
-- error: \[1\]i32

let main (n: i32) : [1]i32 =
  loop xs = replicate 1 0i32 for _i < n do
    xs ++ xs
