fun [int] main([int] a, [int] b) =
  concatMap(fn [int] ([int] r) =>
              map(op+(1), r),
            a, b)
