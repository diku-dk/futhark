module type MONOID = {
  type t

  val neutral: t
  val combine: t -> t -> t
}
