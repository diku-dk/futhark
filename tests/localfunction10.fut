-- Polymorphic local function with size annotations - that's easy to
-- mess up!
-- ==
-- input { [[1f32]] } output { [[1f32]] }

def main (kr: [][]f32): [][]f32 =
  let f 'a [r][c] (arr: [r][c]a): [r][c]a =
    flatten arr |> unflatten
  in f kr
