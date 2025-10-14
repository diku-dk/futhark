-- Host-only operations block the migration of whole statements.
-- ==
-- structure gpu {
--   GPUBody 0
-- }

#[noinline]
def hostonly 'a (x: a) : a =
  -- This function can only be run on host.
  let arr = opaque [x]
  in arr[0]

entry case_if (A: [5]i64) : i64 =
  if A[0] == 0
  then hostonly 42
  else A[1]

entry case_while (A: [5]i64) : i64 =
  loop x = A[0]
  while x < 1000 do
    x * (A[hostonly x % 5] + 1)

entry case_for (A: [5]i64) : i64 =
  loop x = 0
  for i < A[0] do
    x * (A[hostonly x % 5] + 1)
