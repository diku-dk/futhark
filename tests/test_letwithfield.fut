-- Test basic record field update
def main (x: i32) : {a: i32, b: i32} =
  let rec = {a = 1, b = 2}
  let rec.a = x
  in rec

-- Test nested field access
def nested: {inner: {x: i32}} =
  let rec = {inner = {x = 0}}
  let rec.inner.x = 42
  in rec