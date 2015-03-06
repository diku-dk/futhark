fun {[real], [int]} main(int n, [real] a1, [int] a2) =
  let b = zip(a1,a2) in
  let {first, rest} = split( (n), b) in
  unzip(first)
