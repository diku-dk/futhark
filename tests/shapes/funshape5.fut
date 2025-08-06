-- ==
-- error: Entry point functions may not be polymorphic

def main indices (cs: *[](i32, i32)) j =
  map (\k -> (indices[j], k)) <| drop (j + 1) indices
