fun [[int]] main([[int]] input) =
  let x = scan(fn [int] ([int] a, [int] b) =>
                 map(+, zip(a, b)),
               iota(3), input) in
  map(fn [int] ([int] r) =>
        map(+ (2), r),
      x)
