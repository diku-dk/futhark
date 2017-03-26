-- Error if we try to access a non-existent field.
-- ==
-- error: field.*c

let main() =
  let r = {a=1,b=2}
  in #c r
