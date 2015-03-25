fun [int] main ([[int]] xss) =
  map (fn int ([int] xs ) =>
         reduce(op+, 0, xs)
      , xss)
