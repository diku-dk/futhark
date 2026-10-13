-- A record value must not have more fields than its corresponding
-- type.
-- ==
-- error: unshared fields

def main () =
  let r: {a: i32} = {a = 0, b = 0}
  in 0
