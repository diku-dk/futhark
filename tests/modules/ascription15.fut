module type mt = {
  type sum [n] [m] = ([n]bool, [m]bool, [n + m]bool)
}

module m : mt = {
  type sum [n] [m] = ([n]bool, [m]bool, [n + m]bool)
}
