module A = SomeFunctor({
  type t = (int, int)

  fun id (x : t) : t = x
})
