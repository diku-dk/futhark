-- ==
-- error: "t"

module type A = {
  module R: {type t = t}
}
