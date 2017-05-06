module A = SomeFunctor({
  type t = (int, int)

  let id (x : t) : t = x
})
