-- Some intermediate constants may not be live in the functions.
-- Handle this gracefully.  We can't test whether the memory is
-- actually deallocated during initialisation (except through manual
-- inspection), but we can at least check that this isn't fused
-- unexpectedly.
-- ==
-- structure { Screma 2 }

let xs = map (+3) (iota 1000)
let ys = copy xs with [4] = 0
let v = i64.sum ys

let main a = a + v
