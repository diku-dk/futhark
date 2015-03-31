fun [int] main ([[int]] xss) =
  map (fn int ([int] xs ) =>
         reduce(+, 0, xs)
      , xss)
