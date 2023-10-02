def main (x: i64) (y: i64) : [x]i64 =
  let b = y + 2 in
  let rs = tabulate x (\ii ->
  loop acc = 0 for i < x do
    let a = b + acc
    in acc + a + ii / i
  )
  in map2 (+) rs <| reverse rs

-- === Expected output of analysis:
