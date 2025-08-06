module type lys = {
  type~ state
  val event : i64 -> state -> state
}

module lys : lys = {
  type~ state = {arr: []i64}

  def event x (s: state) =
    s with arr = iota x
}
