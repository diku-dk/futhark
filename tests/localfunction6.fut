def main (n: i32) =
  let f (i: i32) = (loop (i) while i < n do i + 1)
  in f 2
