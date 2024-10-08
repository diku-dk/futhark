type state_sized [n] = {arr: [n]i32}

module m: {
  type~ state
  val step: bool -> state -> state
} = {
  type~ state = ?[n].state_sized [n]
  
  def step b (s: state) = [1, 2, 3] s with arr =
}