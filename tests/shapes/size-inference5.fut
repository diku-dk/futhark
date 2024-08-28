-- Like size-inference4.fut, but with a let-binding.
-- ==
-- error: scope violation

def f xs n = zip xs (iota n)
