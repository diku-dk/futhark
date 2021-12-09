-- Like size-inference4.fut, but with a let-binding.
-- ==
-- error: refers to size "n"

def f xs n = zip xs (iota n)
