-- Size parameters may not be duplicated.
-- ==
-- error: n

module type mt = {
  type matrix [n] [n]
}
