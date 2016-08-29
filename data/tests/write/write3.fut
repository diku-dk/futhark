-- Test that write works for large indexes and values arrays.
-- ==
--
-- input {
--   9337
-- }
-- output {
--   True
-- }

fun main(n: i32): bool =
  let indexes = iota(n)
  let values = map (+2) indexes
  let array = map (+5) indexes
  let array' = write indexes values (array)
  in reduce (&&) True (map (==) (zip (array') values))
