-- ==
-- error: field.*c

def main () =
  let r = {a = 1, b = 2}
  let r.c = 1
  in r