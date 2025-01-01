-- Cannot unify a type parameter with a type bound in an outer scope.
-- ==
-- error: "b".*scope

def f x =
  let g 'b (y: b) = if true then y else x.0
  let (_, _: i32) = x
  in g
