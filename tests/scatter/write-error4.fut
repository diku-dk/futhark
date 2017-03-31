-- Fail if the indexes arrays elements do not consist of just integers.
-- ==
-- error:

let main(array: *[n]i32): [n]i32 =
  write ([3.1, 8.3]) ([9, 3]) array
