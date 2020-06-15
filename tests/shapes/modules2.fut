module type mt = {
  type^ t [n] = [n]i32 -> i32
}

module m : mt = {
  type^ t [n] = [n]i32 -> i32
}
