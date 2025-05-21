-- Derived from #2273, but is not the entirety of that bug.

def unit (s: (i64, i64)) : [0][s.0]() = []

def quux (s: i64) : [0][s + 1]() = []

entry velocity = (unit (0, 1), quux 0)
