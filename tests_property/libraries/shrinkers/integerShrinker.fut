module integerlShrinkers (I: integral) = {
  type t = I.t

  def shrinker (x: t) (random: i32): t =
    let tactic = random % 2
    in if tactic == 0 then
      -- Use the division function from the module I
      I.(x / i32 2)
    else
      if I.(x >= i32 0) then
        I.(x - i32 1)
      else
        I.(x + i32 1)
}
