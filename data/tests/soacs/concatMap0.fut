fun [int] main([int] a, [int] b) =
  concatMap(fn [int] ([int] r) =>
              map(+1, r),
            a, b)
