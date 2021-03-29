-- Size-lifted types must not unify with nonlifted types.
-- ==
-- error: array element

let packunpack 'a (f: i64 -> a) : a =
  (tabulate 10 f)[0]

let apply 'a '~b (f: i64 -> a -> b) (x: a) =
  packunpack (\i -> f i x)

let main (x: i32) = apply replicate x
