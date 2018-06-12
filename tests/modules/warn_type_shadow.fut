-- ==
-- warning: Inclusion shadows type

module type has_t = {
  type t
}

module type also_has_t = {
  type t
}

module type mt = {
  include has_t
  include also_has_t
}
