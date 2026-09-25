-- A function whose result type is one of its own type parameters cannot
-- manufacture an abstract value: by parametricity, what it returns came from
-- an argument.  So it must not stop the result from being unique - however the
-- function value reaches the application.
-- ==
-- input { [1,2,3] }
-- output { [1,2,3] }

module pm (M: {type~ t}) = {
  def viaid (x: *M.t) : *M.t = id x
  def vialet (x: *M.t) : *M.t = let my_id = id in my_id x
  def viapipe (x: *M.t) : *M.t = x |> id
}

module m = pm {
  type~ t = []i32
}

entry main (xs: []i32) : *[]i32 =
  m.viapipe (m.vialet (m.viaid (copy xs)))
