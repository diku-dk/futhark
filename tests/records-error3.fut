-- A record value must have at least the fields of its corresponding
-- type.
-- ==
-- error: unshared fields

def main () =
  let r: {a: i32, b: i32} = {a = 0}
  in 0
