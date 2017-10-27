-- | A module type describing a monoid.
module type monoid = {
  type t

  -- Neutral element.
  val ne: t

  -- Associative operation.
  val op: t -> t -> t
}
