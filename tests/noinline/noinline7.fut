-- Based on #2419.
--
-- A noninlined function that references a top level computation. Actually we
-- use an intermediate function to make it even more tricky.
-- ==
-- input { [1i64, 0i64, 2i64] }
-- output { [2, 1, 3] }
-- structure { /Apply 1 /Screma/Apply 1 /ArrayLit 1 }

def A : []i32 = #[opaque] [1, 2, 3]

#[noinline]
def f (i: i64) = A[i]

#[noinline]
def g (i: i64) = f i

entry main (is: []i64) = map g is
