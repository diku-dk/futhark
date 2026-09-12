-- If the function passed to "|>" does *not* have a unique return type,
-- then its closure aliases must still propagate to the result.
-- ==
-- error: aliases the free variable "global"

def global : []i64 = [1, 2, 3]

def main (xs: []i64) = xs |> (\(_: []i64) -> global)
