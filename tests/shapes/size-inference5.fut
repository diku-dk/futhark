-- Like size-inference4.fut, but with a let-binding.
-- ==
-- error: refers to size `n`

let f xs n = zip xs (iota n)
