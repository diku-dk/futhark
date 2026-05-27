module integerlShrinkers (I: integral) = {
  type t = I.t

  def shrinker (x: t) (random: u64) : t =
    let tactic = random % 2
    in if tactic == 0
       then -- Use the division function from the module I
            I.(x / u64 2)
       else if I.(x >= u64 0)
       then I.(x - u64 1)
       else I.(x + u64 1)
}
