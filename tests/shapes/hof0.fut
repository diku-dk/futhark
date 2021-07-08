-- A dubious test - what we want to ensure is an absence of too many
-- dynamic casts just after internalisation.
-- ==
-- structure internalised { Assert 3 }

let f [k] 'a (dest: [k]a) (f: [k]a -> [k]a) : [k]a =
  f dest

let operation [n][m] (b: [m]i32) (as: [n][m]i32) =
  copy as with [0] = b

let main xs b = f xs (operation b)
