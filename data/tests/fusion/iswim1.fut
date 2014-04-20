fun [[int]] main([[int]] input) =
  let x = scan(fn [int] ([int] a, [int] b) =>
                 map(op+, zip(a, b)),
               iota(3), input) in
  map(fn [int] ([int] r) =>
        map(op+ (2), r),
      x)
