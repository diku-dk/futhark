-- ==
-- error: constructive

module m
  : {
      type sum [n] [m]
    } = {
  type sum [n] [m] = [n + m]bool
}
