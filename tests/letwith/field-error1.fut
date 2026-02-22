-- ==
-- error: field.*y

def main () =
  let r = {inner = {x = 0}}
  let r.inner.y = 1
  in r