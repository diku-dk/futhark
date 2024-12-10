-- ==
-- error: Documentation comments.*only permitted when preceding declarations
entry main (x: i32) =
  let y =
     | 1337
     -- | 42
  in y
