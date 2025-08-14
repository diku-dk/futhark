-- ==
-- error: Sizes .* do not match

type^ t = ?[n].([n]bool, bool -> [n]bool)

def v x : t =
  let x' = x + 1
  in ( replicate x' true
     , \b -> replicate x b
     )
