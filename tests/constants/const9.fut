-- Some intermediate constants may not be live in the functions.
-- Handle this gracefully.  We can't test whether the memory is
-- actually deallocated during initialisation (except through manual
-- inspection), but we can at least check that this isn't fused
-- unexpectedly.
-- ==
-- structure { Screma 2 }

def xs = map (+ 3) (iota 1000)
def ys = copy xs with [4] = 0
def v = i64.sum ys

def main a = a + v
