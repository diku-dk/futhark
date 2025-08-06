-- ==
-- error: "a".*consumed

def main n =
  let a = iota (n)
  let b = iota (n)
  let i = 0
  in (let a[i] = b[i] in a[i]) + (let b = a in b[i])

-- Bad because of parallel consume-observe collision.
