-- Test that write works for large indexes and values arrays.
-- ==
--
-- input {
--   9337i64
-- }
-- output {
--   true
-- }

def main (n: i64) : bool =
  let indexes = iota (n)
  let values = map (+ 2) indexes
  let array = map (+ 5) indexes
  let array' = scatter array indexes values
  in reduce (&&) true (map2 (==) array' values)
