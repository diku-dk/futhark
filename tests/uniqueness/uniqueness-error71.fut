-- As uniqueness-error70.fut, but the lambda is applied by a function
-- whose return type is not unique, so the global really is returned.
-- ==
-- error: aliases the free variable "global"

def global : []i64 = [1, 2, 3]

def main (n: i64) = (\(_: i64) -> global) n
