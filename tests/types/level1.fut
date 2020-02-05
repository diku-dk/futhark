-- Cannot unify a type parameter with a type bound in an outer scope.
-- ==
-- error: "b".*scope

let f x =
  let g 'b (y: b) = if true then y else x.1
  let (_, _: i32) = x
  in g
